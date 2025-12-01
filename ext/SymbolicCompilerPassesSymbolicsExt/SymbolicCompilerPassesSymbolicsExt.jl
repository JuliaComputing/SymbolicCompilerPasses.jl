module SymbolicCompilerPassesSymbolicsExt

using Symbolics, LinearAlgebra

@register_array_symbolic LinearAlgebra.triu(x::AbstractArray) begin
    size = size(x)
    eltype = eltype(x)
end

@register_array_symbolic LinearAlgebra.mul!(x::AbstractArray, y::AbstractArray, z::AbstractArray, α, β) begin
    size = size(x)
    eltype = eltype(x)
end
@register_symbolic Base.copy(x)
@register_array_symbolic Base.copy(x::AbstractArray) begin
    size = size(x)
    eltype = eltype(x)
end

end
