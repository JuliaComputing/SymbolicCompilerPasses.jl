using Pkg, Test, SafeTestsets

@testset begin
	# Optimization
	@safetestset "MatmulAdd Optimization" begin include("mul5_opt.jl") end
	@safetestset "Literal Small Array Allocation" begin include("array_literal.jl") end
	@safetestset "Ldiv Factorization Optimization" begin include("ldiv_opt.jl") end
	@safetestset "Apply Multiple Rules" begin include("multiple.jl") end
end