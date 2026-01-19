# SymbolicCompilerPasses.jl 

[![Join the chat at https://julialang.zulipchat.com #sciml-bridged](https://img.shields.io/static/v1?label=Zulip&message=chat&color=9558b2&labelColor=389826)](https://julialang.zulipchat.com/#narrow/stream/279055-sciml-bridged)


[![Build Status](https://github.com/JuliaComputing/SymbolicCompilerPasses.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/JuliaComputing/SymbolicCompilerPasses.jl/actions/workflows/ci.yml)


[![ColPrac: Contributor's Guide on Collaborative Practices for Community Packages](https://img.shields.io/badge/ColPrac-Contributor%27s%20Guide-blueviolet)](https://github.com/SciML/ColPrac)
[![SciML Code Style](https://img.shields.io/static/v1?label=code%20style&message=SciML&color=9558b2&labelColor=389826)](https://github.com/SciML/SciMLStyle)

Compiler Pass Plugins for Symbolic Expressions. Designed to work with array heavy code.

## ModelingToolkit Integration

```julia
using Multibody
import ModelingToolkit as MTK

t = Multibody.t
world = Multibody.world

@named joint = Revolute(n = Float64[0, 0, 1], isroot = true);
@named body = Body(; m = 1, isroot = false, r_cm = [0.5, 0, 0])

connections = [
    connect(world.frame_b, joint.frame_a)
    connect(joint.frame_b, body.frame_a)
]

@named model = System(connections, t, systems=[world, joint, body])
ssys = multibody(model)

D = Differential(t)

prob = ODEProblem(ssys, defs, (0, 3.35),
        optimize = MTK.SCP_BASIC
)
```


A list of passes can be passed as well.

```julia
import SymbolicCompilerPasses as SC

prob = ODEProblem(ssys, defs, (0, 3.35),
        optimize = [SC.LDIV_RULE,
                    SC.HVNCAT_STATIC_RULE,
                    SC.MB_VIEW_RULE,
                    SC.MATMUL_ADD_RULE]
)
```

Lower level API gives more granular control over applying passes to standard symbolic expressions.

```julia
julia> using SymbolicUtils

julia> using SymbolicUtils.Code

julia> import SymbolicUtils as SU

julia> import SymbolicCompilerPasses as SC

julia> @syms A[1:3, 1:3] B[1:3, 1:3] C[1:3, 1:3] D[1:3, 1:3] E[1:3, 1:3]
(A, B, C, D, E)

julia> expr = A * B + C
C + A*B

julia> current = Code.cse(expr)
Let(Union{Assignment, DestructuredArgs}[Assignment(var"##cse#1", A*B), Assignment(var"##cse#2", var"##cse#1" + C)], var"##cse#2", false)

julia> toexpr(current)
quote
    var"##cse#1" = (*)(A, B)
    var"##cse#2" = (+)(var"##cse#1", C)
    var"##cse#2"
end

julia> state = Code.CSEState();

julia> optimized = Code.apply_optimization_rule(current, state, SC.MATMUL_ADD_RULE)
Let(Union{Assignment, DestructuredArgs}[Assignment(var"##mul5_temp#1", SymbolicCompilerPasses.get_from_cache(C)), Assignment(var"##mul5_temp#1", LinearAlgebra.mul!(var"##mul5_temp#1", A, B, 1, 1)), Assignment(var"##mul5_temp#1", var"##mul5_temp#1")], var"##mul5_temp#1", false)

julia> toexpr(optimized)
quote
    var"##mul5_temp#1" = (SymbolicCompilerPasses.get_from_cache)(C)
    var"##mul5_temp#1" = (LinearAlgebra.mul!)(var"##mul5_temp#1", A, B, 1, 1)
    var"##mul5_temp#1" = var"##mul5_temp#1"
    var"##mul5_temp#1"
end
```
