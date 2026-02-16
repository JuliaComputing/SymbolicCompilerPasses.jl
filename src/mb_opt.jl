function detect_small_views(expr::Code.Let, state)
    matches = []
    for (i, p) in enumerate(expr.pairs)
        r = rhs(p)
        iscall(r) || continue
        if operation(r) === view
            arr, inds... = arguments(r)
            myt = find_term(inds[1], expr)
            is_small_hvncat(size(Code.rhs(myt))...) || continue
            push!(matches, (idx = i, expr = r))
        end
    end
    matches
end

function construct_type(dims)
    # if length(dims) == 1
    #     return Core.apply_type(SVector, dims[1])
    # else
    #     return Core.apply_type(SVector, Tuple(dims))
    # end
    Core.apply_type(SVector, length(dims))
end

function find_term(target, expr::Code.Let)
    filter(expr.pairs) do p
        Code.lhs(p) === target
    end |> only
end

function transform_view(expr, match_data, state)
    new_pairs = []
    idxs = Set(getproperty.(match_data, :idx))
    transformations = Dict()
    for match in match_data
        idx = match.idx
        r = match.expr
        T = symtype(r)
        V = vartype(r)
        arr, inds... = arguments(r)
        t = term(construct_type, inds[1])
        transformations[idx] = Term{V}(t, [r], type = T)
    end

    for (i, p) in enumerate(expr.pairs)
        if i in idxs
            new_rhs = transformations[i]
            push!(new_pairs, Code.Assignment(lhs(p), new_rhs))
        else
            push!(new_pairs, p)
        end
    end

    Code.Let(new_pairs, expr.body, expr.let_block)
end


const MB_VIEW_RULE = OptimizationRule(
    "MB_VIEW_RULE",
    detect_small_views,
    transform_view,
    10,
)