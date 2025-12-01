function search_variables!(buf, expr::Code.Let)
    rhs_buf = Set()
    lhs_buf = Set()
    search_variables!.(Ref(rhs_buf), rhs.(expr.pairs))
    search_variables!.(Ref(lhs_buf), lhs.(expr.pairs))
    union!(buf, setdiff(rhs_buf, lhs_buf))
end

function detect_triu(expr, f, state)
    args = search_variables(expr)
    arg_counts = Dict(arg => count_occurrences(arg, expr) for arg in args)
    triu_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        op = operation(r)
        op === f || return false

        args = arguments(r)
        length(args) == 1 || return false

        arg = args[1]
        symtype(arg) <: AbstractMatrix || return false

        # The var"##cse#" variables must get counted at least twice (once in LHS and a second time in RHS)
        # TODO: needs to detect provenance better; see expr7
        get!(arg_counts, arg) do
            count_occurrences(arg, expr)
        end == 1 || return false

        true
    end

    triu_candidates = expr.pairs[triu_candidates_idx]

    matches = map(triu_candidates_idx, triu_candidates) do idx, candidate
        A = arguments(rhs(candidate))[1]
        TriuMatched(A, candidate, idx)
    end

    f = filter(!isnothing, matches)
    isempty(f) ? nothing : f
end

struct TriuMatched{Ta, S} <: Code.AbstractMatched
    A::Ta
    candidate::S
    idx::Int
end

transform_triu(expr, f!, ::Nothing, state) = expr
function transform_triu(expr, f!, matches, state)

    new_pairs = []
    transformed_idxs = getproperty.(matches, :idx)
    for (idx, pair) in enumerate(expr.pairs)
        if idx in transformed_idxs
            match = matches[findfirst(==(idx), transformed_idxs)]
            A = match.A
            candidate = match.candidate

            push!(new_pairs, Assignment(lhs(candidate), term(f!, A,)))
        else
            push!(new_pairs, pair)
        end
        
    end

    Code.Let(new_pairs, expr.body, false)
end


function GenericRule(name, f, f!, priority)
    OptimizationRule(
        name,
        (expr, state) -> detect_triu(expr, f, state),
        (expr, matches, state) -> transform_triu(expr, f!, matches, state),
        priority
    )
end

const TRIU_RULE = GenericRule("triu", LinearAlgebra.triu, LinearAlgebra.triu!, 8)
const TRIL_RULE = GenericRule("tril", LinearAlgebra.tril, LinearAlgebra.tril!, 8)
const NORMALIZE_RULE = GenericRule("normalize", LinearAlgebra.normalize, LinearAlgebra.normalize!, 8)
# const CONJ_RULE = GenericRule("conj", LinearAlgebra.conj, LinearAlgebra.conj!, 8)
   
function triu_opt(expr, state::CSEState)
    # Try to apply optimization rules
    optimized = apply_optimization_rules(expr, state, TRIU_RULE)
    if optimized !== nothing
        return optimized
    end
   
    # If no optimization applied, return original expression
    return expr
end