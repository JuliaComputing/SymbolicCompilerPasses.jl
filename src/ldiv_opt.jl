const FACTORIZATION_CACHE = WeakKeyDict()

struct LdivMatch{Ta, Tb, S <: Assignment, P <: AbstractString} <: AbstractMatched
    A::Ta
    B::Tb
    ldiv_candidate::S
    ldiv_idx::Int
    pattern::P
end

"""
    detect_ldiv_pattern(expr::Code.Let, state) -> Vector{LdivMatch}

Detect patterns of the form `result = A \\ B` where both A and B are arrays.
"""
function detect_ldiv_pattern(expr::Code.Let, state)
    ldiv_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        op = operation(r)
        op === (\) || return false

        args = arguments(r)
        length(args) == 2 || return false

        all_arrays = all(y -> y <: AbstractArray, symtype.(args))
        all_arrays || return false

        A, B = args
        validate_ldiv_shapes(A, B)
    end

    ldiv_candidates = expr.pairs[ldiv_candidates_idx]

    matches = map(ldiv_candidates_idx, ldiv_candidates) do idx, candidate
        A, B = arguments(rhs(candidate))
        LdivMatch(A, B, candidate, idx, "A \\ B")
    end

    f = filter(!isnothing, matches)
    isempty(f) ? nothing : f
end

"""
    validate_ldiv_shapes(A, B) -> Bool

Validate that A and B have compatible shapes for linear solve.
For A \\ B:
- A must be square: (n, n)
- B must have n rows: (n, m) or (n,)
"""
function validate_ldiv_shapes(A, B)
    A_shape = shape(A)
    B_shape = shape(B)

    (A_shape isa Unknown || B_shape isa Unknown) && return false
    (any(x -> x isa Unknown, A_shape) || any(x -> x isa Unknown, B_shape)) && return false

    # A must be square
    symtype(A) <: AbstractMatrix || return false
    A_shape[1] == A_shape[2] || return false

    A_shape[2] == B_shape[1]
end

"""
    count_uses_after(var, expr::Code.Let, after_idx::Int) -> Int

Count how many times `var` is used after `after_idx` in the IR.
"""
function count_uses_after(var, expr::Code.Let, after_idx::Int)
    count_occurrences(var, expr.pairs[(after_idx + 1):end]) + count_occurrences(var, expr.body)
end

"""
    count_occurrences(target, expr) -> Int

Recursively count occurrences of `target` in `expr`.
"""
function count_occurrences(target, expr::Code.Let)
    count_occurrences(unwrap(target), expr.pairs) + count_occurrences(unwrap(target), expr.body)
end

function count_occurrences(target, expr::Code.Assignment)
    count_occurrences(unwrap(target), rhs(expr)) + count_occurrences(unwrap(target), lhs(expr))
end

function count_occurrences(target, expr::AbstractVector)
    sum(e -> count_occurrences(target, e), expr, init = 0)
end

function count_occurrences(target, expr)
    target = unwrap(target)
    expr = unwrap(expr)
    if issym(expr)
        target === expr ? 1 : 0
    elseif iscall(expr)
        issame = target === expr ? 1 : 0
        issame + sum(arg -> count_occurrences(target, arg), unwrap.(arguments(expr)), init = 0)
    else
        0
    end
end

count_occurrences(target, expr::Code.SetArray) = count_occurrences(target, expr.arr)
count_occurrences(target, expr::Code.ForLoop) = count_occurrences(target, expr.body)

"""
    is_safe_to_optimize_ldiv(match::LdivMatch, expr::Code.Let) -> Bool

Check if it's safe to optimize this ldiv operation.

Safety conditions:
1. A is not used after the ldiv (we'll overwrite it with factorization)
2. B is not used after the ldiv (we'll overwrite it with result)
3. Result is used at least once

Returns is_safe
"""
function is_safe_to_optimize_ldiv(match::LdivMatch, expr::Code.Let)
    # Check: A not used after ldiv (will be factorized in-place)
    a_uses = count_uses_after(match.A, expr, match.ldiv_idx)
    if a_uses > 0
        return false
    end

    # Check: B not used after ldiv (will be overwritten with result)
    b_uses = count_uses_after(match.B, expr, match.ldiv_idx)
    if b_uses > 0
        return false
    end

    # Check: Result is used
    result_var = Code.lhs(match.ldiv_candidate)
    result_uses = count_uses_after(result_var, expr, match.ldiv_idx)
    if result_uses == 0 && expr.body !== result_var
        return false
    end

    # Check: A and B don't alias
    if match.A === match.B
        return false
    end

    return true
end

function get_factorization(A)
    qr_A = get!(FACTORIZATION_CACHE, A) do 
        qr(A)
    end

    qr_A
end

const LINEARSOLVE_LIB = Ref(false)

ldiv_transformation(x, bool::Bool) = ldiv_transformation(x, Val(bool))
ldiv_transformation(x, ::Nothing) = ldiv_transformation(x, Val(false))
function ldiv_transformation(safe_matches, ::Val{false})
    @warn "Backsolve may be sped up by adding LinearSolve.jl.
    In order to enable this optimization, add LinearSolve.jl to your environment.
    To opt-out of using LinearSolve, set SymbolicCompilerPasses.LINEARSOLVE_LIB[] = false." maxlog=Inf

    # Build transformation
    transformations = Dict{Int, Code.Assignment}()

    for match in safe_matches
        A, B = match.A, match.B
        result_var = Code.lhs(match.ldiv_candidate)
        T = Code.vartype(B)

        qr_A = Term{T}(
            get_factorization,
            [A];
            type=Code.symtype(A)
        )

        B_cache = Term{T}(get_from_cache, [B]; type=symtype(B))

        # Create: result = ldiv!(A, B)
        ldiv_call = Term{T}(
            LinearAlgebra.ldiv!,
            [qr_A, B_cache];
            type=Code.symtype(B)
        )

        ldiv_assignment = Code.Assignment(result_var, ldiv_call)
        transformations[match.ldiv_idx] = ldiv_assignment
    end
    transformations
end

"""
    transform_to_ldiv_inplace(expr::Code.Let, match_data, state) -> Code.Let

Transform `result = A \\ B` to:
    result = ldiv!(A, B)

This performs in-place linear solve, overwriting B with the result.
"""
function transform_to_ldiv_inplace(expr::Code.Let, match_data::AbstractVector, state)
    # Validate all matches
    safe_matches = filter(match_data) do match
        is_safe = is_safe_to_optimize_ldiv(match, expr)
        is_safe
    end

    isempty(safe_matches) && return expr

    transformations = ldiv_transformation(safe_matches, LINEARSOLVE_LIB[])

    # Apply transformations
    new_pairs = map(enumerate(expr.pairs)) do (i, pair)
        get(transformations, i, pair)
    end

    return Code.Let(new_pairs, expr.body, expr.let_block)
end
transform_to_ldiv_inplace(expr::Code.Let, match_data::Nothing, state) = expr

# Create the optimization rule
const LDIV_RULE = OptimizationRule(
    "Ldiv",
    detect_ldiv_pattern,
    transform_to_ldiv_inplace,
    5  # Lower priority than matmul
)

function ldiv_opt(expr, state::CSEState)

    # Try to apply optimization rules
    optimized = apply_optimization_rules(expr, state, [LDIV_RULE])
    if optimized !== nothing
        return optimized
    end

    # If no optimization applied, return original expression
    return expr
end