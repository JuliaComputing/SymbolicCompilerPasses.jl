module SymbolicCompilerPasses

using LinearAlgebra
using PreallocationTools
using SymbolicUtils
import SymbolicUtils: symtype, vartype, Sym, BasicSymbolic, Term, iscall, operation, arguments, maketerm, Const
import SymbolicUtils.Code: Code, OptimizationRule, substitute_in_ir, apply_optimization_rules, AbstractMatched,
    Assignment, CSEState, lhs, rhs, apply_substitution_map, issym, isterm, toexpr,
    _is_array_of_symbolics, MakeArray, shape
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

end # module SymbolicCompilerPasses
