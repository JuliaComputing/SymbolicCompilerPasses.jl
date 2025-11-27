is_orthogonal_matrix(A) = A * A' ≈ I(size(A, 1))
is_orthogonal_type(::Rotations.Rotation) = true
is_orthogonal_type(x) = false


# ie = SU.Code.IfElse(SU.Const{SymReal}(true), SU.Code.Let([], nothing, false), SU.Code.Let([], nothing, false))
# ie = SU.Code.IfElse(is_orthogonal, SU.Code.Let([], nothing, false), SU.Code.Let([], nothing, false))
# toexpr(ie)

function detect_orthogonal_matrix(expr, state::Code.CSEState)
    orthogonal_candidates_idx = findall(expr.pairs) do x
        r = rhs(x)
        iscall(r) || return false
        op = operation(r)
        op === Symbol("is_orthogonal") || return false

        args = arguments(r)
        length(args) == 1 || return false

        arg = args[1]
        symtype(arg) <: AbstractMatrix || return false

        true
    end

    orthogonal_candidates = expr.pairs[orthogonal_candidates_idx]

    matches = map(orthogonal_candidates_idx, orthogonal_candidates) do idx, candidate
        A = arguments(rhs(candidate))[1]
        (A, candidate, idx, "is_orthogonal(A)")
    end

    f = filter(!isnothing, matches)
    isempty(f) ? nothing : f
end