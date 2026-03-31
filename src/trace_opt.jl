using LinearAlgebra: tr, dot

"""
    TraceMatch{AT, BT} <: AbstractMatched

Represents a matched trace pattern for optimization.

Fields:
- `trace_call`: The tr(...) call
- `mul_call`: The expression inside tr()
- `trace_idx::Int`: Index for tr(...) in the Let block
- `mul_idx::Int`: Index for the mul chain in the Let block
- `pattern::Symbol`: Type of pattern (:tr_product, :tr_chain)
"""
struct TraceMatch{AT, BT} <: AbstractMatched
    trace_call::AT
    mul_call::BT
    trace_idx::Int
    mul_idx::Int
    pattern::Symbol
end

"""
    is_matrix_product(expr) -> Bool

Check if expression is a matrix multiplication (A * B or A * B * C).
"""
function is_matrix_product(expr)
    iscall(expr) || return false
    op = operation(expr)
    (op === *) || return false

    args = arguments(expr)
    length(args) >= 2 || return false

    # Check all arguments are arrays
    all(arg -> symtype(arg) <: AbstractArray, args) || return false

    return true
end

"""
    detect_trace_patterns(expr::Code.Let, state::Code.CSEState) -> Union{Nothing, Vector{TraceMatch}}

Detects trace patterns that can be optimized:

1. tr(A * B) → dot(A, B')
2. tr(A * B * C) → dot(A', C * B) with optimal ordering
3. tr(A * B * C * D) → similar optimization for longer chains
"""
function detect_trace_patterns(expr::Code.Let, state::Code.CSEState)
    tr_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        all_arrays = symtype(r) <: AbstractArray
        is_mul = operation(r) === tr || operation(r) === LinearAlgebra.tr
        all_arrays && is_mul
    end
    tr_candidates = expr.pairs[tr_candidates_idx]

    mul_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        all_arrays = symtype(r) <: AbstractArray
        is_mul = operation(r) === *
        all_arrays && is_mul
    end
    mul_candidates = expr.pairs[mul_candidates_idx]

    @show arguments.(rhs.(mul_candidates))

    matches = TraceMatch[]
    for (t_idx, t) in zip(tr_candidates_idx, tr_candidates)
        for (m_idx, m) in zip(mul_candidates_idx, mul_candidates)
            if nameof(lhs(m)) in nameof.(arguments(rhs(t)))
                push!(matches, TraceMatch(
                    t,
                    m,
                    t_idx,
                    m_idx,
                    :tr_product
                ))
            end
        end
    end

    @show mul_candidates

    # check we dont need the multiplication elsewhere
    matches = filter(matches) do m
        mul = lhs(m.mul_call)
        count_uses_after(mul, expr, m.mul_idx) == 1
    end

    isempty(matches) ? nothing : matches
end

"""
    find_optimal_trace_chain_order(matrices::Vector) -> (Int, Int)

For tr(A * B * C * ...), finds optimal pairing to minimize computation.

Returns indices (i, j) such that computing matrices[i] * matrices[j] first,
then taking dot product with remaining matrix, is optimal.

For tr(A * B * C) with dimensions (m×n, n×p, p×m):
- dot(A', C * B) costs: n×p×m + m×n = O(npm + mn)
- dot((A*B)', C) costs: m×n×p + m×p = O(mnp + mp)
Choose the cheaper one based on matrix dimensions.
"""
function find_optimal_trace_chain_order(matrices::Vector)
    n = length(matrices)

    if n == 2
        # tr(A * B) → dot(A, B')
        return (1, 2)
    elseif n == 3
        # tr(A * B * C)
        # Options:
        # 1. dot(A', C * B) - multiply last two first
        # 2. dot((A * B)', C) - multiply first two first
        # 3. dot(B', A' * C') - multiply first and last

        # For simplicity, use heuristic: multiply adjacent pairs
        # Here we choose option 1: dot(A', C * B)
        return (2, 3)  # Multiply matrices[2] * matrices[3] first
    else
        # General case: use heuristic
        # Multiply rightmost pair first
        return (n-1, n)
    end
end

"""
    transform_trace_to_dot(expr::Code.Let, matches, state::Code.CSEState)

Transforms matched trace patterns to optimized dot products.

Transformations:
```julia
# Before
result = tr(A * B)

# After
temp = B'
result = dot(A, temp)

# Before
result = tr(A * B * C)

# After
temp1 = C * B
temp2 = A'
result = dot(temp2, temp1)
```
"""
function transform_trace_to_dot(expr::Code.Let, matches, state::Code.CSEState)
    matches === nothing && return expr

    transformations = Dict()
    mul_idxs = getproperty.(matches, :mul_idx)
    trace_idxs = getproperty.(matches, :trace_idx)
    transformed_idxs = Set([getproperty.(matches, :mul_idx); getproperty.(matches, :trace_idx)])

    muls = getproperty.(matches, :mul_call)

    counter = 1

    for match in matches
        T = vartype(lhs(match.trace_call))
        new_assignments = Code.Assignment[]
        num_matrices = length(arguments(rhs(match.mul_call)))

        if num_matrices == 2
            # tr(A * B) → dot(A, B')
            A, B = arguments(rhs(match.mul_call))

            # Create transpose of B
            temp_sym = Symbol("##trace_opt_temp#", counter)
            counter += 1
            temp_var = Sym{T}(temp_sym; type=symtype(B))

            transpose_call = Term{T}(transpose, [B]; type=symtype(B))
            transpose_assignment = Assignment(temp_var, transpose_call)

            # Create dot product
            dot_call = Term{T}(dot, [A, temp_var]; type=symtype(lhs(match.trace_call)))
            dot_assignment = Assignment(lhs(match.trace_call), dot_call)

            push!(new_assignments, transpose_assignment)
            push!(new_assignments, dot_assignment)

        elseif num_matrices == 3
            # tr(A * B * C) → dot(A', C * B)
            A, B, C = arguments(rhs(match.mul_call))

            # Create C * B
            temp1_sym = Symbol("##trace_opt_temp#", counter)
            counter += 1
            temp1_var = Sym{T}(temp1_sym; type=symtype(C))

            product_call = Term{T}(*, [C, B]; type=symtype(C))
            product_assignment = Assignment(temp1_var, product_call)

            # Create A'
            temp2_sym = Symbol("##trace_opt_temp#", counter)
            counter += 1
            temp2_var = Sym{T}(temp2_sym; type=symtype(A))

            transpose_call = Term{T}(transpose, [A]; type=symtype(A))
            transpose_assignment = Assignment(temp2_var, transpose_call)

            # Create dot(A', C*B)
            dot_call = Term{T}(dot, [temp2_var, temp1_var]; type=symtype(lhs(match.trace_call)))
            dot_assignment = Assignment(lhs(match.trace_call), dot_call)

            push!(new_assignments, product_assignment)
            push!(new_assignments, transpose_assignment)
            push!(new_assignments, dot_assignment)

        else
            # General case: tr(A * B * C * D * ...)
            # Use heuristic: compute rightmost product, then dot with leftmost transpose
            matrices = arguments(rhs(match.mul_call))

            if length(matrices) == 4
                # tr(A * B * C * D) → dot(A', D * C * B)
                # Compute D * C * B as a chain
                A = matrices[1]
                rest = matrices[2:end]

                # Build product of rest (B * C * D)
                temp_sym = Symbol("##trace_opt_temp#", counter)
                counter += 1
                temp_var = Sym{T}(temp_sym; type=symtype(rest[end]))

                product_call = Term{T}(*, rest; type=symtype(rest[end]))
                product_assignment = Assignment(temp_var, product_call)

                # Create A'
                temp2_sym = Symbol("##trace_opt_temp#", counter)
                counter += 1
                temp2_var = Sym{T}(temp2_sym; type=symtype(A))

                transpose_call = Term{T}(transpose, [A]; type=symtype(A))
                transpose_assignment = Assignment(temp2_var, transpose_call)

                # Create dot
                dot_call = Term{T}(dot, [temp2_var, temp_var]; type=symtype(lhs(match.trace_call)))
                dot_assignment = Assignment(lhs(match.trace_call), dot_call)

                push!(new_assignments, product_assignment)
                push!(new_assignments, transpose_assignment)
                push!(new_assignments, dot_assignment)
            else
                # Very long chains: fall back to simple optimization
                # Just use dot(first, product_of_rest')
                A = matrices[1]
                rest = matrices[2:end]

                temp_sym = Symbol("##trace_opt_temp#", counter)
                counter += 1
                temp_var = Sym{T}(temp_sym; type=symtype(rest[end]))

                product_call = Term{T}(*, rest; type=symtype(rest[end]))
                product_assignment = Assignment(temp_var, product_call)

                # Transpose the product
                temp2_sym = Symbol("##trace_opt_temp#", counter)
                counter += 1
                temp2_var = Sym{T}(temp2_sym; type=symtype(temp_var))

                transpose_call = Term{T}(transpose, [temp_var]; type=symtype(temp_var))
                transpose_assignment = Assignment(temp2_var, transpose_call)

                # Create dot
                dot_call = Term{T}(dot, [A, temp2_var]; type=symtype(lhs(match.trace_call)))
                dot_assignment = Assignment(lhs(match.trace_call), dot_call)

                push!(new_assignments, product_assignment)
                push!(new_assignments, transpose_assignment)
                push!(new_assignments, dot_assignment)
            end
        end

        transformations[match.trace_idx] = new_assignments
    end

    # Reconstruct Let block with transformations
    new_pairs = []
    for (i, pair) in enumerate(expr.pairs)
        if i in mul_idxs
            continue
        end
        if i in trace_idxs
            append!(new_pairs, transformations[i])
        else
            push!(new_pairs, pair)
        end
    end

    return Code.Let(new_pairs, expr.body, expr.let_block)
end

const TRACE_OPT_RULE = OptimizationRule(
    "TraceOpt",
    detect_trace_patterns,
    transform_trace_to_dot,
    12  # Priority - run after matmul but before in-place
)

"""
    trace_opt(expr::Code.Let, state::Code.CSEState)

Main entry point for trace optimization pass.
"""
function trace_opt(expr::Code.Let, state::Code.CSEState)
    optimized = apply_optimization_rules(expr, state, TRACE_OPT_RULE)
    if optimized !== nothing
        return optimized
    end
    return expr
end
