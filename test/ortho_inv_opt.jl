using Revise, BenchmarkTools

using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using Symbolics

using LinearAlgebra
using Rotations
using Test

function has_ortho_opt(expr::Code.Let)
    return any(expr.pairs) do assignment
        rhs_expr = Code.rhs(assignment)
        has_ortho_opt(rhs_expr)
    end
end
has_ortho_opt(expr::Code.IfElse) = has_ortho_opt(expr.ifbody) || has_ortho_opt(expr.elsebody)

function has_ortho_opt(expr)
    if SU.iscall(expr)
        op = SU.operation(expr)
        return op === transpose || op === SC.is_orthogonal_type
    end
    false
end

function check_ortho_opt(expr, A, B)
    current = SU.Code.cse(expr)
    toexpr(current)

    optimized = SC.ortho_inv_opt(current, SU.Code.CSEState())
    # return optimized
    return toexpr(optimized)

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


# @testset "Orthogonal Matrices: inv -> transpose" begin
    @syms A[1:3, 1:3] B[1:3, 1:3] C[1:3, 1:3] D[1:3, 1:3] E[1:3, 1:3]
    Ao = SU.setmetadata(A, SC.IsOrthogonal, true)


    expr = inv(Ao) * B
    check_ortho_opt(expr, Ao, B)

    expr2 = inv(A) * B
    check_ortho_opt(expr2, A, B)

    expr3 = inv(A) * B * inv(Ao)
    check_ortho_opt(expr3, A, B)
    
    expr4 = inv(Ao * B) + B
    check_ortho_opt(expr4, A, B)
# end


function all_rules(mod)
    f = filter(x -> getproperty(SC, x) isa Code.OptimizationRule, names(SC, all = true))
    getproperty.(Ref(SC), f)
end

rules = all_rules(SC)


expr3 = inv(A) * B * inv(Ao) + C


ir = Code.cse(expr3)
optimized = ir
for rule in sort(rules, by = r -> r.priority)
    st = SU.Code.CSEState()
    cand = Code.apply_optimization_rules(optimized, st, rule)
    if !isnothing(cand)
        optimized = cand
    end
end
toexpr(optimized)

current_fun = Func([A, B, C], [], ir)
optimized_fun = Func([A, B, C], [], optimized)

current_f = eval(toexpr(current_fun))
optimized_f = eval(toexpr(optimized_fun))

N = 32
a = rand(N, N)
b = rand(N, N)
c = rand(N, N)
R_euler = RotMatrix{N, Float64}(rand(N, N))
res1 = invokelatest(current_f, R_euler, b, c)
res2 = invokelatest(optimized_f, R_euler, b, c)
res1 ≈ res2

@btime invokelatest($current_f, $a, $b, $c);
@btime invokelatest($optimized_f, $a, $b, $c);