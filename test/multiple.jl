using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using LinearAlgebra
using Symbolics
using Test

using Rotations

function test_codegen(expr, rules, args...)
    current = SU.Code.cse(expr)
    optimized = SU.Code.apply_optimization_rules(current, SU.Code.CSEState(), rules)

    current_expr = Func([args...], [], current)
    optimized_expr = Func([args...], [], optimized)

    current_f = eval(toexpr(current_expr))
    optimized_f = eval(toexpr(optimized_expr))

    N = 3
    test_args = map(args) do arg
        if getmetadata(arg, SC.IsOrthogonal, false) == true
            RotXYZ((rand([0,1]) for x in 1:size(arg, 1))...)
        else
            rand(size(arg)...)
        end
    end

    current_res = @invokelatest current_f(test_args...)
    optimized_res = @invokelatest optimized_f(test_args...)

    @test isapprox(current_res, optimized_res, rtol=1e-10, atol=1e-10)
end


@testset "Combined Optimizations" begin
    @syms A[1:3, 1:3] B[1:3, 1:3] C[1:3, 1:3] D[1:3, 1:3] E[1:3, 1:3] 

    P = A \ B
    expr = P + C * D
    test_codegen(expr, [SC.LDIV_RULE, SC.MATMUL_ADD_RULE], A, B, C, D)

    # note that LDIV_RULE should not apply because A is reused
    P = A \ B
    expr2 = P * E + A
    test_codegen(expr2, [SC.LDIV_RULE, SC.MATMUL_ADD_RULE], A, B, E)

    @syms R[1:3, 1:3]
    Ro = setmetadata(R, SC.IsOrthogonal, true)
    P = A \ B
    # expr3 = P + C * inv(Ro)
    expr3 = tril(P) + C * inv(Ro)
    test_codegen(expr3, [SC.LDIV_RULE, SC.MATMUL_ADD_RULE, SC.ORTHO_INV_RULE], A, B, C, Ro)
end
