module SymbolicCompilerPasses

using LinearAlgebra
using PreallocationTools
using SymbolicUtils
import SymbolicUtils: symtype, vartype, Sym, BasicSymbolic, Term, iscall, operation, arguments, maketerm, Const, shape, isterm, unwrap,
                    is_function_symbolic, is_called_function_symbolic, getname, Unknown
import SymbolicUtils.Code: Code, OptimizationRule, substitute_in_ir, apply_optimization_rules, AbstractMatched,
    Assignment, CSEState, lhs, rhs, apply_substitution_map, issym, isterm, toexpr,
    _is_array_of_symbolics, MakeArray, shape
import SymbolicUtils: search_variables, search_variables!
using StaticArrays

function bank(dic, key, value)
    if haskey(dic, key)
        dic[key] = vcat(dic[key], value)
    else
        dic[key] = value
    end
end

include("matmuladd.jl")
include("hvncat_static_opt.jl")
include("ldiv_opt.jl")
include("la_opt.jl")

end # module SymbolicCompilerPasses
