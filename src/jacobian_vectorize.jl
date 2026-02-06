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

function hold_eq(eq, inputs)
    if SU.isconst(eq) # || SU.issym(eq)
        return true
    elseif SU.issym(eq)
        # TODO: some variables seem to get used but don't have a definition
        # How do you handle those?
        if eq in inputs
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

function expand_eq(du_expr, inputs, expr)
    args = arguments(du_expr)
    defs = [find_definition(arg, expr).def.rhs for arg in args]
    sub_map = Dict(arg => def for (arg, def) in zip(args, defs))

    new_eq = Code.substitute_in_ir(du_expr, sub_map)

    new_args = reduce(vcat, arguments.(defs))
    expand_eq(new_eq, new_args, inputs, expr)
end
function expand_eq(du_expr, args, inputs, expr)
    if all(hold_eq(a, inputs) for a in args)
        return du_expr
    end

    args_to_sub = filter(a -> !hold_eq(a, inputs), args)
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
    new_args = reduce(vcat, hold_eq(def, inputs) ? def : arguments(def) for def in defs)
    expand_eq(new_eq, new_args, inputs, expr)
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
            arr, expr_idxs = arguments(expr)
            c = arr === var
            if c
                return true, arr
            else
                if issym(var)
                    return false, nothing
                elseif operation(var) === getindex
                    arr_var, var_idxs = arguments(var)
                    if arr === arr_var && isempty(setdiff(expr_idxs, var_idxs))
                        return true, expr
                    end
                end
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
    end

    return eq, isempty(nls) ? Num(0) : +(nls...)
end

"""
constant here means constant in Us; could depend on other variables
should not be recursive
"""
function find_constant_terms!(const_terms, expr, U)
    # @show expr
    # if U isa AbstractArray
    #     U = first(U)
    # end
    # has_var, _ = contains_variable(expr, U)
    # # @warn has_var
    # if !has_var
    #     push!(const_terms, expr)
    #     return const_terms
    # end
    # args = arguments(simplify(expr))
    # @show args

    # for a in args
    #     has_variable, t = contains_variable(a, U)
    #     if !has_variable
    #         push!(const_terms, a)
    #     end
    # end
    zero_dict = Dict(collect(U) .=> 0)
    const_expr = simplify(Symbolics.substitute(expr, zero_dict))
    push!(const_terms, const_expr)
    const_terms
end

function find_linear_terms!(linear_terms, expr, U)
    args = collect(Set(get_expanded_args(expr)))
    args_filtered = filter(args) do a
        has_var, t = contains_variable(a, U)
        if has_var
            return true
        else
            return false
        end
    end
    # TODO: why is args_filtered empty
    # TODO: handle vector of U
    args_filtered = U
    zero_dict = Dict(collect(U) .=> 0)
    gs = Symbolics.gradient(expr, args_filtered)
    for (a, g) in zip(args_filtered, gs)
        # Trying to catch linear terms here is very error prone
        # substitute all variables with 0 should yield all constants
        linear_terms[a] = simplify(Symbolics.substitute(g, zero_dict))
        # linear_terms[cartesian_to_linear(a)] = simplify(Symbolics.substitute(g, zero_dict))
    end
    linear_terms
end

function find_nonlinear_terms!(nonlinear_terms, expr, U)
    # Needs to take care of trig fns
    # Needs to take care of coupled terms u[i] * u[j]
    # Needs to take care of squuared/ cubed/ polynomial terms

    # @show expr

    push!(nonlinear_terms, simplify(expr))
    nonlinear_terms
end

function separate_linear_nonlinear_constant(eq, U)
    const_terms = []
    linear_terms = Dict()
    nonlinear_terms = []
    find_constant_terms!(const_terms, eq, U)
    const_sum = isempty(const_terms) ? Num(0) : +(const_terms...)
    eq = simplify(eq - const_sum)

    find_linear_terms!(linear_terms, eq, U)
    linear_sum = isempty(linear_terms) ? Num(0) : +([key * val for (key, val) in pairs(linear_terms)]...)
    eq = simplify(eq - linear_sum)

    find_nonlinear_terms!(nonlinear_terms, eq, U)
    nonlinear_sum = isempty(nonlinear_terms) ? Num(0) : +(nonlinear_terms...)

    return const_terms, linear_terms, nonlinear_sum
end

function separate_linear_nonlinear(J::AbstractArray, U)
    L = zeros(Symbolics.Num, size(J))
    NL = zeros(Symbolics.Num, size(J))
    C = zeros(Symbolics.Num, size(J))

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
    C = J

    L, NL = separate_linear_nonlinear(J, U)
    VectorizedJacobianMatched(L, NL, C, U, size(U))
end

struct VectorizedJacobianMatched{TL, TNL, TC, TU, S} <: AbstractMatched
    L::TL
    NL::TNL
    C::C
    U::TU
    shape::S
end

function improve_inputs(inputs)
    improved = Set()
    for inp in inputs
        if SU.iscall(inp) && operation(inp) == getindex
            arr, _ = arguments(inp)
            push!(improved, arr)
        else
            push!(improved, inp)
        end
    end
    improved
end

function construct_jacobian(eqs, vars)
    J = Symbolics.jacobian(eqs, vars)
    return J
end

function cartesian_to_linear(x)
    if !(iscall(x) && operation(x) == getindex)
        error("Input must be a getindex call")
    end

    arr, idx... = arguments(x)
    cartesian_to_linear((idx...,), size(arr))
end

function cartesian_to_linear(indices::Tuple, dims::Tuple)
    @assert length(indices) == length(dims) "Indices and dimensions must have same length"

    linear_idx = indices[1]
    stride = 1

    for d in 2:length(indices)
        stride *= dims[d-1]
        linear_idx += (indices[d] - 1) * stride
    end

    return linear_idx
end

function linear_to_cartesian(idx, x)
    x[linear_to_cartesian(idx, size(x))...]
end
function linear_to_cartesian(idx, dims::Tuple)
    idx = Symbolics.value(idx)                                                                                             
                                                                                                                                                
    n_dims = length(dims)                                                                                                                         
    indices = Vector{Int}(undef, n_dims)                                                                                                          
                                                                                                                                                
    remaining = idx - 1                                                                                                                      
                                                                                                                                                
    for d in 1:n_dims                                                                                                                             
        indices[d] = mod(remaining, dims[d]) + 1
        remaining = div(remaining, dims[d])
    end

    return Tuple(indices)
end

function extract_coeffs(eq, vars, expr)
    eq = simplify(eq)
    vars = Set(collect(vars))
    eq_args = get_expanded_args(simplify(eq))
    eq_args_filtered = filter(eq_args) do a
        if issym(a) || SU.isconst(a)
            false
        elseif SU.iscall(a) && operation(a) == getindex
            return a in vars
        end
        false
    end
    eq_args_filtered = collect(Set(eq_args_filtered))
    js = cartesian_to_linear.(eq_args_filtered)

    const_terms, linear_terms, nonlinear_sum = separate_linear_nonlinear_constant(eq, eq_args_filtered)
    js, const_terms, linear_terms, nonlinear_sum
end

"""
Construct sparse jacobian using expression expansion and not full dense jacobian
"""
function construct_jacobian(eqs, vars, expr)
    # J[i,j] = ∂(du[i])/∂(u[j])
    A = zeros(Symbolics.Num, length(eqs), length(vars))
    NL = zeros(Symbolics.Num, length(eqs))
    C = zeros(Symbolics.Num, length(eqs))

    for i in 1:length(eqs)
        js, const_terms, linear_terms, nonlinear_sum = extract_coeffs(eqs[i], vars, expr)
        for (k, j) in enumerate(js)
            A[i, Symbolics.value(j)] = linear_terms[linear_to_cartesian(j, vars)]
        end
        NL[i] = nonlinear_sum
        C[i] = isempty(const_terms) ? Num(0) : +(const_terms...)
    end
    return A, NL, C
end

function detect_jacobian(expr::Code.Let, state)
    inputs = improve_inputs(search_variables(expr))
    eqs = candidate_templates(expr)
    # J[i,j] = ∂(du[i])/∂(u[j])
    expanded_eqs = expand_eq.(eqs, Ref(inputs), Ref(expr))
    full_expanded_eqs = replace_idx_vars_with_vals.(expanded_eqs, Ref(expr))
    L, NL, C = construct_jacobian(vec(full_expanded_eqs), Main.U, expr)
    VectorizedJacobianMatched(L, NL, C, Main.U, size(Main.U))
end

function transform_jacobian(expr::Code.Let, match_data::VectorizedJacobianMatched, state)
    # DU = L * U + NL(U)

    linear_terms = match_data.L * vec(match_data.U)
    nonlinear_terms = match_data.NL
    const_terms = match_data.C
    val = linear_terms + nonlinear_terms + const_terms
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
