module SCPLinearSolveExt

using SymbolicUtils
using SymbolicUtils.Code
using LinearSolve
using LinearAlgebra
import SymbolicCompilerPasses: ldiv_transformation, SymbolicCompilerPasses, get_factorization, get_from_cache, FACTORIZATION_CACHE

SymbolicCompilerPasses.LINEARSOLVE_LIB[] = true

function linear_solve(A, B)
    linsolve = get_factorization(A, B)
    # linsolve = init(LinearSolve.LinearProblem(A, B))
    linsolve.b = B
    sol = solve!(linsolve)
    return sol.u
end

function get_factorization(A, B)
    get!(FACTORIZATION_CACHE, A) do
        prob = LinearSolve.LinearProblem(A, B)
        linsolve = init(prob)
    end
end


function ldiv_transformation(safe_matches, ::Val{true})
    @info "Using LinearSolve.jl for in-place backsolve optimizations.
    In order to opt-out of using LinearSolve, set SymbolicCompilerPasses.LINEARSOLVE_LIB[] = false." maxlog=Inf
     # Build transformation
    transformations = Dict{Int, Code.Assignment}()

    rejected_matches = []
    for match in safe_matches
        A, B = match.A, match.B
        result_var = Code.lhs(match.ldiv_candidate)
        T = Code.vartype(B)

        # Create: result = ldiv!(A, B)
        if Code.symtype(B) <: AbstractVector
            ldiv_call = Code.Term{T}(
                linear_solve,
                [A, B];
                type=Code.symtype(B)
            )
        else
            @warn "Skipping LinearSolve optimization for match as B is not a vector." maxlog=Inf
            push!(rejected_matches, match)
            continue
        end

        ldiv_assignment = Code.Assignment(result_var, ldiv_call)
        transformations[match.ldiv_idx] = ldiv_assignment
    end
    fallback_transformations = ldiv_transformation(rejected_matches, Val(false))
    merge(transformations, fallback_transformations)
end


end