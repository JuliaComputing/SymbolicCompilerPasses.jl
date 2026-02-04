is_orthogonal_matrix(A) = A * A' ≈ I(size(A, 1))
is_orthogonal_type(x) = begin
    if issym(x)
        return getmetadata(x, IsOrthogonal, false)
    else
        return false
    end
    false
end

struct IsOrthogonal end

struct InvMatched{T, C} <: AbstractMatched
    A::T
    candidate::C
    idx::Int
end

function detect_orthogonal_matrix(expr, state::Code.CSEState)
    # var_to_ortho = Dict()
    # ortho_to_var = Dict()
    # for v in search_variables(expr)
    #     @show symtype(v)
    #     bank(var_to_ortho, v, getmetadata(v, IsOrthogonal, false))
    #     bank(ortho_to_var, getmetadata(v, IsOrthogonal, false), v)
    # end

    # if !haskey(ortho_to_var, true)
    #     @warn "not found any metadata arrays"
    #     return nothing
    # end

    idxs = findall(expr.pairs) do p
        r = rhs(p)
        iscall(r) || return false
        op = operation(r)
        if op === inv
            args = arguments(r)
            length(args) == 1 || return false
            getmetadata(args[1], IsOrthogonal, false) == true || return false
            return true
        end
        false
    end

    candidates = expr.pairs[idxs]

    matches = map(idxs, candidates) do idx, candidate
        A = arguments(rhs(candidate))[1]
        InvMatched(A, candidate, idx)
    end

    f = filter(!isnothing, matches)
    isempty(f) ? nothing : f
end

function transform_inv_optimization(expr, matches, state::Code.CSEState)

    expr_copy = deepcopy(expr)
    map(matches) do match
        A = match.A
        if getmetadata(A, IsOrthogonal, false) == true
            expr_copy.pairs[match.idx] = Code.Assignment(
                lhs(match.candidate),
                transpose(A)
            )
        else
            t = term(is_orthogonal_type, A)
            # code = IfElse(
            #     t,
            #     transpose(A),
            #     inv(A)
            # )
            code = ifelse(t, transpose(A), inv(A))
            expr_copy.pairs[match.idx] = Code.Assignment(
                lhs(match.candidate),
                code
            )
        end
    end
    expr_copy
end

const ORTHO_INV_RULE = OptimizationRule(
    "Ortho_Inv",
    detect_orthogonal_matrix,
    transform_inv_optimization,
    10
)

function ortho_inv_opt(expr, state::Code.CSEState)

    # Try to apply optimization rules
    optimized = apply_optimization_rules(expr, state, [ORTHO_INV_RULE])
    if optimized !== nothing
        return optimized
    end

    # If no optimization applied, return original expression
    return expr
end