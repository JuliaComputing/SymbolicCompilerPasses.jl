using Pkg, Test, SafeTestsets

@testset begin
	# Optimization
	@safetestset "MatmulAdd Optimization" begin include("mul5_opt.jl") end
end

