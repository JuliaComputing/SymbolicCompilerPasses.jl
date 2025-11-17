"""
    HvncatMatch <: AbstractMatched

Represents a detected hvncat operation that should be converted to StaticArray.
"""
struct HvncatMatch{V, E, D, DA, T} <: AbstractMatched
    lhs_var::V                      # Variable being assigned
    hvncat_expr::E                  # The hvncat call
    dims::D                         # Inferred dimensions (rows, cols)
    dims_arg::DA                    # Original dims argument from hvncat
    elements::T                     # Flattened elements
    assignment::Code.Assignment     # Original assignment
    assignment_idx::Int             # Index in IR
    pattern::String
end

"""
    is_hvncat(expr) -> Bool

Check if an expression is an hvncat (horizontal-vertical-n-concatenation) operation.
This is what Julia uses for array literals like [1 2; 3 4].
"""
function is_hvncat(expr)
    iscall(expr) || return false

    op = operation(expr)

    # Check for hvncat or related array construction operations
    # hvncat is the general form: hvncat(dims, row_first, values...)
    # Also check for hcat, vcat which are simpler forms
    op_name = op isa SymbolicUtils.Mapreducer ? :identity : nameof(op)


    return op_name in [:hvncat, :hcat, :vcat, :typed_hvncat, :typed_hcat, :typed_vcat, :array_literal]
end

"""
    resolve_dims_argument(dims_arg, expr::Code.Let) -> Union{Nothing, Any}

Resolve a dimensions argument which might be an alias to a CSE variable.

For example, if dims_arg is var"##cse#1" and the IR contains:
  var"##cse#1" = (3,)
Then this returns (3,).
"""
function resolve_dims_argument(dims_arg, expr::Code.Let)
    # If it's a symbol, try to find its definition in the IR
    if issym(dims_arg)
        # Search for the assignment that defines this variable
        for pair in expr.pairs
            if lhs(pair) === dims_arg
                # Found the definition, return the RHS
                return rhs(pair)
            end
        end
    end

    # Otherwise return as-is
    return dims_arg
end

"""
    extract_tuple_elements(tuple_expr) -> Union{Nothing, Vector}

Extract elements from a tuple expression.
Handles both literal tuples and Const-wrapped tuples.
"""
function extract_tuple_elements(tuple_expr)
    @show tuple_expr
    @show typeof(tuple_expr)
    # @show arguments(tuple_expr)
    # @show operation(tuple_expr)
    SymbolicUtils.isconst(tuple_expr) && return tuple_expr
    # Check if it's a call to tuple constructor
    if iscall(tuple_expr) && operation(tuple_expr) === tuple
        return arguments(tuple_expr)
    end

    # Check if it's a Const wrapping a tuple
    if iscall(tuple_expr)
        op = operation(tuple_expr)
        if op isa Type && op <: Const
            const_val = arguments(tuple_expr)[1]
            if const_val isa Tuple
                return collect(const_val)
            end
        end
    end

    return nothing
end

extract_tuple_elements(n::Tuple{M}) where {M} = (n..., 1)
extract_tuple_elements(n::Tuple{M,N}) where {M,N} = n

"""
    infer_hvncat_dimensions(op_name::Symbol, args, expr::Code.Let) -> Union{Nothing, Tuple{Int,Int}}

Infer dimensions from hvncat/hcat/vcat operation.

Returns (rows, cols) or nothing if dimensions cannot be determined.

The expr parameter is needed to resolve aliased dimension arguments.
"""
function infer_hvncat_dimensions(op_name::Symbol, args, expr::Code.Let)
    if op_name == :hvncat
        # hvncat format: hvncat(dims, row_first, values...)
        # dims is a tuple like (2, 3) for 2 rows, 3 cols per row
        # row_first is Bool indicating layout

        length(args) >= 3 || return nothing

        dims_arg = args[1]
        row_first_arg = args[2]
        values = args[3:end]

        # Resolve alias if dims_arg is a CSE variable
        resolved_dims = resolve_dims_argument(dims_arg, expr)

        @show dims_arg, resolved_dims
        # resolved_dims !== nothing && return resolved_dims
        # Try to extract tuple elements
        dim_elements = extract_tuple_elements(resolved_dims)
        @show typeof(resolved_dims)
        # @show Main.@which extract_tuple_elements(resolved_dims)
        @show dim_elements

        if dim_elements !== nothing && length(dim_elements) >= 1
            # Extract dimension values
            # Could be (n,) for vector or (m, n) for matrix
            dims = []
            for elem in dim_elements
                if elem isa Integer
                    push!(dims, elem)
                elseif iscall(elem) && operation(elem) isa Type && operation(elem) <: Const
                    # Const-wrapped integer
                    val = arguments(elem)[1]
                    if val isa Integer
                        push!(dims, val)
                    else
                        # return nothing
                    end
                else
                    # return nothing
                end
            end

            @show dims
            if length(dims) == 1
                # Vector case: (n,) means n rows, 1 column
                return (dims[1], 1)
            elseif length(dims) == 2
                # Matrix case: (m, n)
                return (dims[1], dims[2])
            end
        end

    elseif op_name == :hcat
        # hcat concatenates horizontally: [a b c] -> hcat(a, b, c)
        # Result is 1 row, N cols
        return (1, length(args))

    elseif op_name == :vcat
        # vcat concatenates vertically: [a; b; c] -> vcat(a, b, c)
        # Result is N rows, 1 col
        return (length(args), 1)

    elseif op_name == :typed_hvncat
        # typed_hvncat(T, dims, row_first, values...)
        length(args) >= 4 || return nothing
        dims_arg = args[2]

        if iscall(dims_arg) && operation(dims_arg) === tuple
            dim_values = arguments(dims_arg)
            if length(dim_values) == 2
                if all(d -> d isa Integer, dim_values)
                    return (dim_values[1], dim_values[2])
                end
            end
        end

    elseif op_name == :typed_hcat
        # typed_hcat(T, values...)
        return (1, length(args) - 1)

    elseif op_name == :typed_vcat
        # typed_vcat(T, values...)
        return (length(args) - 1, 1)
    end

    return nothing
end

"""
    extract_hvncat_elements(op_name::Symbol, args) -> Vector

Extract the element values from hvncat/hcat/vcat arguments.
"""
function extract_hvncat_elements(op_name::Symbol, args)
    if op_name in [:hvncat]
        # Skip dims and row_first, return values
        return args[3:end]
    elseif op_name in [:hcat, :vcat]
        # All args are elements
        return args
    elseif op_name in [:typed_hvncat]
        # Skip type and dims and row_first
        return args[4:end]
    elseif op_name in [:array_literal]
        # Skip dims
        return args[2:end]
    elseif op_name in [:typed_hcat, :typed_vcat]
        # Skip type
        return args[2:end]
    end
    return []
end

"""
    is_small_hvncat(rows::Int, cols::Int) -> Bool

Check if the hvncat dimensions are small enough for StaticArrays.
Limit to 4×4 (16 elements) for optimal performance.
"""
function is_small_hvncat(rows::Int, cols::Int)
    return rows <= 4 && cols <= 4 && rows * cols <= 16
end

"""
    detect_hvncat_pattern(expr::Code.Let, state::Code.CSEState) -> Union{Nothing, Vector{HvncatMatch}}

Detect hvncat/hcat/vcat operations that construct small literal arrays.

Examples of what this detects:
- `A = [1 2; 3 4]` → hvncat((2,2), true, 1, 2, 3, 4)
- `v = [1, 2, 3]` → vcat(1, 2, 3)
- `h = [1 2 3]` → hcat(1, 2, 3)

For small arrays (≤ 4×4), these will be converted to StaticArrays.
"""
function detect_hvncat_pattern(expr::Code.Let, state::Code.CSEState)
    matches = HvncatMatch[]

    for (idx, pair) in enumerate(expr.pairs)
        rhs_expr = rhs(pair)
        lhs_var = lhs(pair)

        @show is_hvncat(rhs_expr)

        # Check if this is an hvncat-like operation
        is_hvncat(rhs_expr) || continue

        op = operation(rhs_expr)
        op_name = nameof(op)
        @show op_name
        args = arguments(rhs_expr)

        # Extract the original dims argument
        dims_arg = if op_name == :hvncat
            args[1]  # For hvncat, dims is the first argument
        elseif op_name == :typed_hvncat
            args[2]  # For typed_hvncat, dims is the second argument (after type)
        elseif op_name == :array_literal
            args[1]
        else
            nothing  # hcat/vcat don't have explicit dims arguments
        end

        # Infer dimensions (pass expr to resolve aliases)
        dims = infer_hvncat_dimensions(op_name, args, expr)
        @show dims

        dims = (3, 1)
        dims === nothing && continue
        @show dims

        # Check if small enough for StaticArrays
        is_small_hvncat(dims[1], dims[2]) || continue
        # error()

        # Extract elements
        elements = extract_hvncat_elements(op_name, args)

        # Verify we have the right number of elements
        expected_count = dims[1] * dims[2]
        length(elements) == expected_count || continue

        push!(matches, HvncatMatch(
            lhs_var,
            rhs_expr,
            dims,
            dims_arg,
            elements,
            pair,
            idx,
            "hvncat literal: $(dims[1])×$(dims[2])"
        ))
    end

    @show matches

    isempty(matches) ? nothing : matches
end

"""
    transform_hvncat_to_static(expr::Code.Let, match_data::Union{Nothing, Vector{HvncatMatch}},
                               state::Code.CSEState) -> Code.Let

Transform hvncat operations to StaticArray constructors.

Converts:
- `A = [1 2; 3 4]` → `A = SMatrix{2,2}(1, 2, 3, 4)`
- `v = [1, 2, 3]` → `v = SVector{3}(1, 2, 3)`
"""
transform_hvncat_to_static(expr, ::Nothing, state::Code.CSEState) = expr
function transform_hvncat_to_static(expr::Code.Let, match_data::Vector{HvncatMatch},
                                   state::Code.CSEState)
    isempty(match_data) && return expr
    @show match_data

    # Build transformation plan
    transformations = Dict{Int, Code.Assignment}()

    for match in match_data
        lhs_var = match.lhs_var
        dims = match.dims
        dims_arg = match.dims_arg
        elements = match.elements
        T = vartype(lhs_var)

        @show dims
        @show dims_arg

        # Create StaticArray constructor using dims_arg directly
        # Extract dimension expressions from dims_arg
        # if dims_arg !== nothing
        if dims == nothing
            # Use getindex to extract dimensions: dims_arg[1], dims_arg[2], etc.
            dim1_expr = maketerm(typeof(dims_arg[1]), getindex, [dims_arg, 1], nothing)

            if dims[2] == 1
                # Column vector: SVector{dims_arg[1]}(elements...)
                static_ctor = Term{T}(
                    maketerm(Type{StaticArrays.SVector}, StaticArrays.SVector, [dim1_expr], nothing),
                    elements;
                    type=symtype(lhs_var)
                )
            else
                # Matrix or row vector: SMatrix{dims_arg[1], dims_arg[2]}(elements...)
                dim2_expr = maketerm(typeof(dims_arg[2]), getindex, [dims_arg, 2], nothing)
                static_ctor = Term{T}(
                    maketerm(Type{StaticArrays.SMatrix}, StaticArrays.SMatrix, [dim1_expr, dim2_expr], nothing),
                    elements;
                    type=symtype(lhs_var)
                )
            end
        else
            # Fallback: use literal dims values for hcat/vcat cases
            if dims[2] == 1
                # Column vector: SVector{n}(elements...)
                n = dims[1]
                static_ctor = Term{T}(
                    StaticArrays.SVector{n},
                    elements;
                    type=symtype(lhs_var)
                )
            elseif dims[1] == 1
                # Row vector: treat as SMatrix{1,n}
                n = dims[2]
                static_ctor = Term{T}(
                    StaticArrays.SMatrix{1, n},
                    elements;
                    type=symtype(lhs_var)
                )
            else
                # Matrix: SMatrix{m,n}(elements...)
                m, n = dims
                static_ctor = Term{T}(
                    StaticArrays.SMatrix{m, n},
                    elements;
                    type=symtype(lhs_var)
                )
            end
        end

        new_assignment = Assignment(lhs_var, static_ctor)
        transformations[match.assignment_idx] = new_assignment
    end

    # Apply transformations using map pattern
    new_pairs = map(enumerate(expr.pairs)) do (i, pair)
        get(transformations, i, pair)
    end

    return Code.Let(new_pairs, expr.body, expr.let_block)
end

"""
    HVNCAT_STATIC_RULE

Optimization rule for converting small hvncat operations to StaticArrays.

Priority: 20 (very high - should run very early before other optimizations)

This pass converts literal array constructions like [1 2; 3 4] into
StaticArrays when they're small enough (≤ 4×4).
"""
const HVNCAT_STATIC_RULE = OptimizationRule(
    "HvncatStatic",
    detect_hvncat_pattern,
    transform_hvncat_to_static,
    20  # Very high priority - run first
)

function literal_static_opt(expr, state::CSEState)

    # Try to apply optimization rules
    optimized = apply_optimization_rules(expr, state, HVNCAT_STATIC_RULE)
    if optimized !== nothing
        return optimized
    end

    # If no optimization applied, return original expression
    return expr
end
