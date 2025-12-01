module SymbolicCompilerPasses

using LinearAlgebra
using PreallocationTools
using SymbolicUtils
import SymbolicUtils: symtype, vartype, Sym, BasicSymbolic, Term, iscall, operation, arguments, maketerm, Const, shape
import SymbolicUtils.Code: Code, OptimizationRule, substitute_in_ir, apply_optimization_rules, AbstractMatched,
    Assignment, CSEState, lhs, rhs, apply_substitution_map, issym
import SymbolicUtils: search_variables, search_variables!

function bank(dic, key, value)
    if haskey(dic, key)
        dic[key] = vcat(dic[key], value)
    else
        dic[key] = value
    end
end

include("matmuladd.jl")
include("ldiv_opt.jl")
include("la_opt.jl")

end # module SymbolicCompilerPasses
