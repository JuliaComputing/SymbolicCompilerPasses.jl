module SymbolicCompilerPassesRotationsExt

using SymbolicCompilerPasses
import SymbolicCompilerPasses: is_orthogonal_type

using Rotations
is_orthogonal_type(::Rotations.Rotation) = true

end