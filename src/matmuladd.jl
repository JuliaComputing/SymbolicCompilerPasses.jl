const PRECACHE = IdDict()

struct MatMulAddMatch{At, Bt, Ct} <: AbstractMatched
    A::At
    B::Bt
    Cs::Ct
    mul_candidate::Code.Assignment
    plus_candidate::Code.Assignment
    mul_idx::Int
    plus_idx::Int
    pattern::String
end

"""
    detect_matmul_add_pattern(expr::Code.Let, state::Code.CSEState) -> Union{Nothing, Vector{MatMulAddMatch}}

Attempts to detect patterns of the form:

```julia
result = A * B + C
```

And replaces them with in-place multiplication and addition:

```julia
copy!(temp, C)
mul!(temp, A, B, 1, 1)
result = temp
```

`A` and `B` must not be aliased.
"""
function detect_matmul_add_pattern(expr::Code.Let, state)
    mul_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        all_arrays = symtype(r) <: AbstractArray
        is_mul = operation(r) === *
        all_arrays && is_mul
    end
    mul_candidates = expr.pairs[mul_candidates_idx]

    plus_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        all_arrays = symtype(r) <: AbstractArray
        is_plus = operation(r) === +
        all_arrays && is_plus
    end
    plus_candidates = expr.pairs[plus_candidates_idx]

    candidates = map(plus_candidates_idx, plus_candidates) do p_idx, p

        map(mul_candidates_idx, mul_candidates) do m_idx, m_v
            if nameof(lhs(m_v)) in nameof.(arguments(rhs(p)))
                (m_idx, m_v) => (p_idx, expr.pairs[p_idx])
            end
        end
    end
    candidates = isempty(candidates) ? candidates : filter(!isnothing, reduce(vcat, candidates))

    all_additive_terms = map(candidates) do c
        arguments(rhs(c[2][2]))
    end |> Iterators.flatten |> Set
    all_multiplicative_terms = Set(lhs.(mul_candidates))
    net_additive_terms = setdiff(all_additive_terms, all_multiplicative_terms)

    matches = MatMulAddMatch[]

    for ((mul_idx, mul_val), (plus_idx, plus_val)) in candidates
        A, B... = arguments(rhs(mul_val))
        Cs = isempty(net_additive_terms) ? continue : [pop!(net_additive_terms)]
        push!(matches, MatMulAddMatch(A, B, Cs, mul_val, plus_val, mul_idx, plus_idx, "A*B + C"))
    end

    isempty(matches) ? nothing : matches, net_additive_terms
end

function get_from_cache(x)
    if haskey(PRECACHE, x)
        v = get_tmp(PRECACHE[x], x)
        copyto!(v, x)
        v
    else
        PRECACHE[x] = DiffCache(copy(x))
        v = get_tmp(PRECACHE[x], x)
        v
    end
end

transform_to_mul5_assignment(expr, ::Tuple{Union{Nothing, AbstractVector{Nothing}, Tuple{Nothing, Nothing}}, <:Any}, state) = expr
function transform_to_mul5_assignment(expr, match_data_, state)
    match_data_, net_additive_terms = match_data_
    match_data_ === nothing && return expr
    Cset = Set(Iterators.flatten(getproperty.(match_data_, :Cs)))
    counter = 1

    m_ = map(match_data_) do match_data

        A, B = match_data.A, match_data.B
        C = pop!(Cset)
        T = vartype(C)

        # Create temporary variable for the result
        temp_var_sym = Symbol("##mul5_temp#", counter)
        counter += 1
        temp_var = Sym{T}(temp_var_sym; type=symtype(C))

        if B isa AbstractVector{<:BasicSymbolic}
            if length(B) == 1
                B = B[1]
            else
                B = Term{T}(*, B, type=symtype(C))
            end
        end

        copy_call = Term{T}(get_from_cache, [C]; type=symtype(C))
        mul_call = Term{T}(LinearAlgebra.mul!,
            [temp_var, A, B, Const{T}(1), Const{T}(1)];
            type=symtype(C))

        # Add assignments to CSE state
        copy_assignment = Assignment(temp_var, copy_call)
        mul_assignment = Assignment(temp_var, mul_call)  # This overwrites temp_var with mul! result
        final_assignment = Assignment(temp_var, temp_var)

        [copy_assignment, mul_assignment, final_assignment]
    end

    transformed_idxs = getproperty.(match_data_, :plus_idx)
    substitution_map = get_substitution_map(match_data_, m_)
    rm_idxs = getproperty.(match_data_, :mul_idx)
    transformations = Dict()
    for (i, mm) in zip(transformed_idxs, m_)
        bank(transformations, i, mm)
    end

    new_pairs = []
    for (i, e) in enumerate(expr.pairs)
        if i in transformed_idxs
            append!(new_pairs, transformations[i])
        elseif i in rm_idxs
            # do nothing/ skip over
            # TODO: handle expr9 when a hanging mul is filtered out
        else
            push!(new_pairs, e)
        end
    end

    bank(substitution_map, last(match_data_).plus_candidate.lhs, collect(Cset))
    bank(substitution_map, last(match_data_).plus_candidate.lhs, collect(net_additive_terms))

    new_let = Code.Let(new_pairs, expr.body, expr.let_block)
    transformed_ir = apply_substitution_map(new_let, substitution_map)

    transformed_ir
end

function get_substitution_map(match_data, transformations)
    dic = Dict()
    @assert length(match_data) == length(transformations)

    for (m, t) in zip(match_data, transformations)
        bank(dic, m.plus_candidate.lhs, t[2].lhs)
    end
    dic
end

const MATMUL_ADD_RULE = OptimizationRule(
    "MatMul+Add",
    detect_matmul_add_pattern,
    transform_to_mul5_assignment,
    10
)

function mul5_opt(expr, state::CSEState)

    # Try to apply optimization rules
    optimized = apply_optimization_rules(expr, state, MATMUL_ADD_RULE)
    if optimized !== nothing
        return optimized
    end

    # If no optimization applied, return original expression
    return expr
end