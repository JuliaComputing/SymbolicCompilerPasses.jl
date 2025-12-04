module SymbolicCompilerPassesRotationsExt

using SymbolicCompilerPasses
import SymbolicCompilerPasses: is_orthogonal_type
is_orthogonal_type(::Rotations.Rotation) = true

end