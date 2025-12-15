"""
    HvncatMatch <: AbstractMatched

Represents a detected hvncat operation that should be converted to StaticArray.
"""
struct HvncatMatch{V, E, D, DA, T} <: AbstractMatched
    lhs_var::V
    hvncat_expr::E
    dims::D
    dims_arg::DA
    elements::T
    assignment::Code.Assignment
    assignment_idx::Int
    pattern::String
end

"""
    is_literal_construct_op(op) -> Bool

Check if an operation is an array concatenation operation.
Uses dispatch on the function type for type-safe checking.
"""
is_literal_construct_op(::typeof(SymbolicUtils.array_literal)) = true
is_literal_construct_op(::typeof(SymbolicUtils.Code.create_array)) = true
is_literal_construct_op(::MakeArray) = true
is_literal_construct_op(x) = false

"""
    is_hvncat(expr) -> Bool

Check if an expression is an hvncat (horizontal-vertical-n-concatenation) operation.
This is what Julia uses for array literals like [1 2; 3 4].
"""
function is_hvncat(expr)
    iscall(expr) || return false
    op = operation(expr)
    return is_literal_construct_op(op)
end
is_hvncat(::AbstractArray) = true

operation(::AbstractArray) = SymbolicUtils.Code.create_array
arguments(x::T) where T <: AbstractArray = [T, eltype(T), Val(ndims(x)), size(x), x]

"""
    get_dims_arg(op, args) -> Union{Nothing, Any}

Extract the dimension argument from concatenation operation arguments.
Dispatches on operation type for type-safe extraction.
"""
get_dims_arg(::typeof(SymbolicUtils.array_literal), args) = args[1]
get_dims_arg(::typeof(SymbolicUtils.Code.create_array), args) = args[4]
get_dims_arg(::Any, args) = nothing  # hcat/vcat don't have explicit dims arguments

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

function unalias(var, expr::Code.Let)
    if issym(var)
        for p in expr.pairs
            if lhs(p) === var
                return iscall(p) ? nothing : rhs(p)
            end
        end
    end
end

function infer_hvncat_dimensions(::typeof(SymbolicUtils.array_literal), args, expr::Code.Let)
    dims_arg = get_dims_arg(SymbolicUtils.array_literal, args)
    val = unalias(dims_arg, expr)
end

function infer_hvncat_dimensions(::typeof(SymbolicUtils.Code.create_array), args, expr::Code.Let)
    args[4]
end
infer_hvncat_dimensions(::Any, args, expr::Code.Let) = nothing

"""
    extract_hvncat_elements(op, args) -> Vector

Extract the element values from concatenation operation arguments.
Dispatches on operation type for type-safe extraction.
"""
extract_hvncat_elements(::typeof(SymbolicUtils.array_literal), args) = args[2:end]
extract_hvncat_elements(::typeof(SymbolicUtils.Code.create_array), args) = vec(args[5])
extract_hvncat_elements(::Any, args) = []

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

        # Check if this is an hvncat-like operation
        is_hvncat(rhs_expr) || continue

        op = operation(rhs_expr)
        args = arguments(rhs_expr)

        # Extract the original dims argument using dispatch
        dims_arg = get_dims_arg(op, args)

        # Infer dimensions using dispatch
        dims = Code.unwrap_const(infer_hvncat_dimensions(op, args, expr))
        dims = length(dims) == 1 ? (dims[1], 1) : dims
        dims === nothing && continue

        # Check if small enough for StaticArrays
        is_small_hvncat(dims[1], dims[2]) || continue

        # Extract elements using dispatch
        elements = extract_hvncat_elements(op, args)

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

    # Build transformation plan
    transformations = Dict{Int, Code.Assignment}()

    for match in match_data
        lhs_var = match.lhs_var
        dims = match.dims
        dims_arg = match.dims_arg
        elements = match.elements
        T = vartype(lhs_var)

        # Fallback: use literal dims values for hcat/vcat cases
        if length(dims) == 2 && dims[2] == 1
            # Column vector: SVector{n}(elements...)
            n = dims[1]
            t = term(Core.apply_type, StaticArrays.SVector, n; type = Any)
            static_ctor = Term{T}(
                t,
                elements;
                type=symtype(lhs_var)
            )
        else
            # Matrix: SMatrix{m,n}(elements...)
            m, n = dims
            t = term(Core.apply_type, StaticArrays.SMatrix, m, n; type = Any)
            static_ctor = Term{T}(
                t,
                elements;
                type=symtype(lhs_var)
            )
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
