using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using LinearAlgebra
using Test

function test_ldiv(expr, args...)
    current = SU.Code.cse(expr)
    optimized = SC.ldiv_opt(current, SU.Code.CSEState())

    current_expr = Func([args...], [], current)
    optimized_expr = Func([args...], [], optimized)

    current_f = eval(toexpr(current_expr))
    optimized_f = eval(toexpr(optimized_expr))

    M = 32
    N = 16
    a = randn(M, M)
    b = randn(M, N)
    c = randn(M, N)

    if length(args) == 2
        current_o = @invokelatest current_f(a, b)
        optimized_o = @invokelatest optimized_f(a, b)
    elseif length(args) == 3
        current_o = @invokelatest current_f(a, b, c)
        optimized_o = @invokelatest optimized_f(a, b, c)
    end
    @test isapprox(current_o, optimized_o, rtol=1e-10, atol=1e-10)
end

@testset "LDiv Factorization" begin
    @syms A[1:3, 1:3] B[1:3, 1:2] C[1:3, 1:2]

    expr = A \ B
    test_ldiv(expr, A, B)

    expr2 = C + (A \ B)
    test_ldiv(expr2, A, B, C)
end