using SymbolicUtils
using SymbolicUtils.Code
import SymbolicUtils as SU
import SymbolicCompilerPasses as SC
using LinearAlgebra
using StaticArrays
using Test

@testset "Literal Small Array Allocation" begin
    @syms myx_vec[1:3]
    literal_vec_expr = Func([myx_vec[1], myx_vec[2], myx_vec[3]], [],
                Let([Assignment(myx_vec, SU.Const{SymReal}([myx_vec[1], myx_vec[2], myx_vec[3]]))], sum(myx_vec), false)
            )
    literal_vec_f = eval(toexpr(literal_vec_expr))


    static_vec_expr_ = SU.Code.cse(literal_vec_expr.body)
    static_vec_expr = SC.literal_static_opt(static_vec_expr_, SU.Code.CSEState())
    static_vec_expr = Func([myx_vec[1], myx_vec[2], myx_vec[3]], [], static_vec_expr)
    static_vec_f = eval(toexpr(static_vec_expr))

    r = rand(3)
    literal_vec_alloc = @allocated literal_vec_f(r...)
    # static_vec_alloc = @allocated static_vec_f(r...)
    # @test static_vec_alloc < literal_vec_alloc

    @syms myx[1:2, 1:2]
    literal_mat_expr = Func([myx[1, 1], myx[2, 1], myx[1, 2], myx[2, 2]], [],
                Let([
                    Assignment(
                        myx,
                        [myx[1, 1] myx[2, 1]; myx[1, 2] myx[2, 2]]),
                    ], sum(myx), false)
            )
    static_mat_expr = SU.Code.cse(literal_mat_expr.body)

    static_mat_expr = SC.literal_static_opt(static_mat_expr, SU.Code.CSEState())
    static_mat_expr = Func([myx], [], static_mat_expr)

    static_mat_f = eval(toexpr(static_mat_expr))
    literal_mat_f = eval(toexpr(literal_mat_expr))

    # throwaway for allocation test
    r2 = rand(2, 2)
    literal_mat_f(r2[1, 1], r2[2, 1], r2[1, 2], r2[2, 2])
    static_mat_f(r2)
    
    literal_mat_alloc = @allocated literal_mat_f(r2[1, 1], r2[2, 1], r2[1, 2], r2[2, 2])
    static_mat_alloc = @allocated static_mat_f(r2)
    @test static_mat_alloc < literal_mat_alloc
end