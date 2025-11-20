using Pkg, Test, SafeTestsets

@testset begin
	# Optimization
	@safetestset "MatmulAdd Optimization" begin include("mul5_opt.jl") end
	@safetestset "Literal Small Array Allocation" begin include("array_literal.jl") end
end

