function find_definition(target, expr::Code.Let)
    idx = findfirst(expr.pairs) do p
        Code.lhs(p) === target
    end
    if !isnothing(idx)
        return (; idx, def = expr.pairs[idx])
    else
        return (; idx = nothing, def = nothing)
    end
end

construct_template(ass::Code.Assignment) = construct_template(ass.rhs)
function construct_template(expr)
    seq = []
    construct_template!(seq, expr)
end

function construct_template!(seq, expr)
    if iscall(expr)
        push!(seq, operation(expr))
        args = arguments(expr)
        for a in args
            construct_template!(seq, a)
        end
        seq
    else
        seq
    end
    seq
end

function basic_groupby(temps, expr)
    groups = Dict()
    for (i, t) in enumerate(temps)
        SC.bank(groups, t, (i, expr.pairs[i]))
    end
    groups
end

function code_flow(expr)
    g = basic_groupby(construct_template.(expr.pairs), expr)
    return g
end

function candidate_templates(expr)
    dus = find_definition.(expr.body, Ref(expr))
    map(dus) do du
        du.def.rhs
    end
end

function hold_eq(eq)
    if SU.isconst(eq) # || SU.issym(eq)
        return true
    elseif SU.issym(eq)
        # TODO: some variables seem to get used but don't have a definition
        # How do you handle those?
        if eq === Main.t || eq === Main.P
            return true
        else
            return false
        end
    else
        if SU.iscall(eq) && operation(eq) == getindex
            return true
        else
            return false
        end
    end
    return false

end

function expand_eq(du_expr, expr)
    args = arguments(du_expr)
    defs = [find_definition(arg, expr).def.rhs for arg in args]
    sub_map = Dict(arg => def for (arg, def) in zip(args, defs))

    new_eq = Code.substitute_in_ir(du_expr, sub_map)

    new_args = reduce(vcat, arguments.(defs))
    expand_eq(new_eq, new_args, expr)
end
function expand_eq(du_expr, args, expr)
    if all(hold_eq(a) for a in args)
        return du_expr
    end

    args_to_sub = filter(a -> !hold_eq(a), args)
    # for a in args_to_sub
    #     fd = @show find_definition(a, expr)
    #     if isnothing(fd.idx)
    #         @warn a
    #         @show SU.issym(a)
    #         @warn du_expr
    #     end
    # end
    defs = [find_definition(arg, expr).def.rhs for arg in args_to_sub]
    sub_map = Dict(arg => def for (arg, def) in zip(args_to_sub, defs))
    new_eq = Code.substitute_in_ir(du_expr, sub_map)
    new_args = reduce(vcat, hold_eq(def) ? def : arguments(def) for def in defs)
    expand_eq(new_eq, new_args, expr)
end

function get_expanded_args(expr)
    args = []
    get_expanded_args!(args, expr)
    return args
end
function get_expanded_args!(args, expr)
    if SU.iscall(expr)
        if operation(expr) == getindex
            push!(args, expr)
            return
        end
        arg_list = arguments(expr)
        for a in arg_list
            get_expanded_args!(args, a)
        end
    end
end

function replace_idx_vars_with_vals(eq, expr)
    if SU.iscall(eq)
        if operation(eq) == getindex
            idx_vars = arguments(eq)[2:end]
            idx_vars = filter(!SU.isconst, idx_vars)
            if all(SU.isconst, idx_vars)
                return eq
            end
            idx_vals = map(idx_vars) do iv
                def = find_definition(iv, expr).def
                def_rhs = Code.rhs(def)
            end
            new_eq = Code.substitute_in_ir(eq, Dict(zip(idx_vars, idx_vals)))
            return new_eq
        else
            arg_list = arguments(eq)
            new_args = map(arg_list) do arg
                replace_idx_vars_with_vals(arg, expr)
            end
            return operation(eq)(new_args...)
        end
    end
    eq
end

function contains_variable(expr, var)
    if SU.issym(expr)
        return expr === var
    elseif SU.isconst(expr)
        return false, nothing
    elseif SU.iscall(expr)
        if operation(expr) === var
            return true, var
        elseif operation(expr) === getindex
            arr, _ = arguments(expr)
            c = arr === var
            if c
                return true, arr
            else
                return false, nothing
            end
        else
            arg_list = arguments(expr)
            for a in arg_list
                if contains_variable(a, var)[1]
                    arg_list_filtered = filter(x -> !(x === a), arg_list)
                    if all(SU.isconst, arg_list_filtered)
                        return true, expr
                    else
                        return true, a
                    end
                end
            end
        end
    end
    false, nothing
end

function separate_linear_nonlinear(eq, U)
    has_U = true
    nls = []
    while has_U
        has_U, ex = contains_variable(eq, U)
        if isnothing(ex)
            break
        end
        eq = eq - ex
        push!(nls, ex)
        # break
    end

    return eq, isempty(nls) ? Num(0) : +(nls...)
end

function separate_linear_nonlinear(J::AbstractArray, U)
    L = zeros(Symbolics.Num, size(J))
    NL = zeros(Symbolics.Num, size(J))

    for i = 1:size(J, 1), j = 1:size(J, 2)
        SU.isconst(J[i, j]) && continue
        lin, nlin = separate_linear_nonlinear(J[i, j], U)
        L[i, j] = lin
        NL[i, j] = nlin
    end

    return L, NL
end

function detect_jacobian_trace(f, DU, U, P, t)
    traced = f(DU, U, P, t)
    # traced_let = Code.cse(traced)
    # eqs = candidate_templates(traced_let)

    # # J[i,j] = ∂(du[i])/∂(u[j])
    # expanded_eqs = expand_eq.(eqs, Ref(traced_let))

    # full_expanded_eqs = replace_idx_vars_with_vals.(expanded_eqs, Ref(traced_let))
    # J = Symbolics.jacobian(vec(full_expanded_eqs), vec(collect(U)))
    J = Symbolics.jacobian(vec(traced), vec(collect(U)))

    L, NL = separate_linear_nonlinear(J, U)
    VectorizedJacobianMatched(J, L, NL, U, size(U))
end

struct VectorizedJacobianMatched{TJ, TL, TNL, TU, S} <: AbstractMatched
    J::TJ
    L::TL
    NL::TNL
    U::TU
    shape::S
end

function detect_jacobian(expr::Code.Let, state)
    eqs = candidate_templates(expr)
    # J[i,j] = ∂(du[i])/∂(u[j])
    expanded_eqs = expand_eq.(eqs, Ref(expr))
    full_expanded_eqs = replace_idx_vars_with_vals.(expanded_eqs, Ref(expr))
    J = Symbolics.jacobian(vec(full_expanded_eqs), vec(collect(Main.U)))
    L, NL = separate_linear_nonlinear(J, Main.U)
    VectorizedJacobianMatched(J, L, NL, Main.U, size(Main.U))
end

function transform_jacobian(expr::Code.Let, match_data::VectorizedJacobianMatched, state)
    # DU = L * U + NL(U)

    linear_terms = match_data.L * vec(match_data.U)
    nonlinear_terms = dropdims(sum(match_data.NL, dims = 2), dims = 2)
    val = linear_terms + nonlinear_terms
    T = SymReal
    res = Term{T}(reshape, [val, match_data.shape]; type = symtype(match_data.U))

    Code.Let([], res, expr.let_block)
end

const JACOBIAN_VECTORIZE_RULE = OptimizationRule(
    "JacobianVectorize",
    detect_jacobian,
    transform_jacobian,
    10  # Medium priority
)
