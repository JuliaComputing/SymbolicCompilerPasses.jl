using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using LinearAlgebra
using Test

brusselator_f(x, y, t) = (((x - 0.3)^2 + (y - 0.6)^2) <= 0.1^2) * (t >= 1.1) * 5.0
limit(a, N) = a == N + 1 ? 1 : a == 0 ? N : a
function brusselator_2d_loop(du, u, p, t)
    A, B, alpha, dx = p
    alpha = alpha / dx^2
    @inbounds for I in CartesianIndices((N, N))
        i, j = Tuple(I)
        x, y = xyd_brusselator[I[1]], xyd_brusselator[I[2]]
        ip1, im1, jp1,
        jm1 = limit(i + 1, N), limit(i - 1, N), limit(j + 1, N),
        limit(j - 1, N)
        du[i, j, 1] = alpha * (u[im1, j, 1] + u[ip1, j, 1] + u[i, jp1, 1] + u[i, jm1, 1] -
                       4u[i, j, 1]) +
                      B + u[i, j, 1]^2 * u[i, j, 2] - (A + 1) * u[i, j, 1] +
                      brusselator_f(x, y, t)
        du[i, j, 2] = alpha * (u[im1, j, 2] + u[ip1, j, 2] + u[i, jp1, 2] + u[i, jm1, 2] -
                       4u[i, j, 2]) +
                      A * u[i, j, 1] - u[i, j, 1]^2 * u[i, j, 2]
    end
    du
end

@testset "Brusselator Vectorized" begin
    N = 32
    xyd_brusselator = range(0, stop = 1, length = N)
    @syms DU[1:N, 1:N, 1:2]::Real U[1:N, 1:N, 1:2]::Real P[1:4]::Real t::Real
    traced_bruss = brusselator_2d_loop(collect(DU), collect(U), P, t)
    current = Code.cse(traced_bruss)

    u_test = rand(size(U)...)
    du_test = zeros(size(U)...)
    p_test = [3.4, 1.0, 1.0, 0.1]
    t_test = 0.5
    brusselator_2d_loop(du_test, u_test, p_test, t_test)

    d = Dict(collect(U) .=> u_test)
    d[P[1]] = p_test[1]
    d[P[2]] = p_test[2]
    d[P[3]] = p_test[3]
    d[P[4]] = p_test[4]
    d[t] = t_test

    let_res = SC.detect_jacobian(current, Code.CSEState())
    optimized = SU.Code.apply_optimization_rule(current, Code.CSEState(), SC.JACOBIAN_VECTORIZE_RULE)

    optimized_expr = Func([U, P, t], [], optimized)

    optimized_f = eval(toexpr(optimized_expr));
    du_optimized = @invokelatest optimized_f(u_test, p_test, t_test)
    @test isapprox(du_test, du_optimized, rtol = 1e-10, atol = 1e-10)
end