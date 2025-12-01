using Revise, BenchmarkTools

using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using LinearAlgebra
using Test
using Symbolics

function has_triu_optimization(ir)
    if ir isa Code.Let
        return any(ir.pairs) do assignment
            rhs_expr = Code.rhs(assignment)
            if SU.iscall(rhs_expr)
                op = SU.operation(rhs_expr)
                return op === LinearAlgebra.triu!
            end
            false
        end
    end
    return false
end

function test_triu(expr, args...)
    current = SU.Code.cse(expr)
    optimized = SC.triu_opt(current, SU.Code.CSEState())

    current_func = Func([args...], [], current)
    optimized_func = Func([args...], [], optimized)

    # return toexpr(current)
    # return toexpr(optimized)

    current_f = eval(toexpr(current_func))
    optimized_f = eval(toexpr(optimized_func))

    N = 3
    test_args = collect(randn(N, N) for _ in 1:length(args))

    current_res = @invokelatest current_f(test_args...)
    optimized_res = @invokelatest optimized_f(test_args...)

    @test isapprox(current_res, optimized_res, rtol=1e-10, atol=1e-10)
end

function test_triu_with_opt(expr, args...)
    test_triu(expr, args...)
    @test has_triu_optimization(SC.triu_opt(SU.Code.cse(expr), Code.CSEState()))
end

function test_triu_without_opt(expr, args...)
    test_triu(expr, args...)
    @test !has_triu_optimization(SC.triu_opt(SU.Code.cse(expr), Code.CSEState()))
end

# @testset "Triu Optimization Benchmark" begin
    @syms A[1:3, 1:3] B[1:3, 1:3] C[1:3, 1:3]

    expr = LinearAlgebra.triu(A) + B
    test_triu_with_opt(expr, A, B)

    expr2 = LinearAlgebra.triu(A + B) + C
    test_triu_without_opt(expr2, A, B, C) # should have optimization

    P = A + B
    expr3 = LinearAlgebra.triu(P) * C
    test_triu_without_opt(expr3, A, B, C) # should have optimization

    expr4 = LinearAlgebra.triu(A) + LinearAlgebra.triu(B)
    test_triu_with_opt(expr4, A, B)

    expr5 = LinearAlgebra.triu(A * B) + C
    test_triu_without_opt(expr5, A, B, C) # should have optimization

    expr6 = LinearAlgebra.triu(A * B + C) + A
    test_triu_without_opt(expr6, A, B, C) # should have optimization

    expr7 = LinearAlgebra.triu(A) + A
    test_triu_without_opt(expr7, A)
# end
