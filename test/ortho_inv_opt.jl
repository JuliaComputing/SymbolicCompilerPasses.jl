using Revise, BenchmarkTools

using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using Symbolics
# using ModelingToolkit
# using ModelingToolkit: t_nounits as t, D_nounits as D

using LinearAlgebra
using Rotations
using Test

function has_ortho_opt(expr::Code.Let)
    return any(expr.pairs) do assignment
        rhs_expr = Code.rhs(assignment)
        if SU.iscall(rhs_expr)
            # op = SU.operation(rhs_expr)
            # return op === transpose || op === SC.is_orthogonal_type
            return has_ortho_opt(rhs_expr)
        elseif rhs_expr isa SU.Code.IfElse
            return has_ortho_opt(rhs_expr.ifbody) || has_ortho_opt(rhs_expr.elsebody)
        end
        false
    end
end

function has_ortho_opt(expr)
     if SU.iscall(expr)
        op = SU.operation(expr)
        return op === transpose || op === SC.is_orthogonal_type
     end
end

function check_ortho_opt(expr, A, B)
    current = SU.Code.cse(expr)
    toexpr(current)

    optimized = SC.ortho_inv_opt(current, SU.Code.CSEState())
    # return optimized
    # return toexpr(optimized)

    @test has_ortho_opt(optimized)

    current_fun = Func([A, B], [], current)
    optimized_fun = Func([A, B], [], optimized)

    current_f = eval(toexpr(current_fun))
    optimized_f = eval(toexpr(optimized_fun))

    a = rand(3,3)
    b = rand(3,3)
    R_euler = RotMatrix{3, Float64}(rand(3, 3))

    # res1 = invokelatest(current_f, a, b)
    # res2 = invokelatest(optimized_f, a, b)
    # @test isapprox(res1, res2, rtol=1e-10, atol=1e-10)

    res1 = invokelatest(current_f, R_euler, b)
    res2 = invokelatest(optimized_f, R_euler, b)
    @test isapprox(res1, res2, rtol=1e-10, atol=1e-10)
end


@testset "Orthogonal Matrices: inv -> transpose" begin
    @syms A[1:3, 1:3] B[1:3, 1:3] C[1:3, 1:3] D[1:3, 1:3] E[1:3, 1:3]
    Ao = SU.setmetadata(A, SC.IsOrthogonal, true)


    expr = inv(Ao) * B
    check_ortho_opt(expr, Ao, B)

    expr2 = inv(A) * B
    c = check_ortho_opt(expr2, A, B)
end




a = rand(3,3)
b = rand(3,3)
# R_euler = RotXYZ(1,2,3)
R_euler = RotMatrix{3, Float64}(rand(3, 3))
res1 = invokelatest(current2_f, a, b)
res2 = invokelatest(optimized2_f, a, b)

res1 = invokelatest(current2_f, R_euler, b)
res2 = invokelatest(optimized2_f, R_euler, b)

@btime $current2_f($R_euler, $b);
@btime $optimized2_f($R_euler, $b);

@code_warntype optimized2_f(R_euler, b)

# @variables mewx(t)[1:3, 1:3] mewy(t)[1:3, 1:3] mewz(t)[1:3, 1:3]
# @syms mewx[1:3, 1:3] mewy[1:3, 1:3] mewz(t)[1:3, 1:3]
# expr = mewx \ mewy
# SU.Code.cse(expr), SU.Code.CSEState()

# SC.count_occurrences(mewx, SU.Code.cse(expr))