module Utils.Ray (
	reflect,
	refract
) where

import Data.Maybe
import Struct.Vector.Vec3
import SceneDescriptor.Attribute.Material.Dielectric

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n *. (2.0 * dot v n))

refract :: Vec3 -> Vec3 -> RefractiveIndex -> Maybe(Vec3)
refract v n niOvernt
	| discriminant > 0.0 = (Just refracted)
	| otherwise = Nothing
	where
		uv = makeUnitVector v
		dt = dot uv n
		discriminant = 1.0 - (niOvernt*niOvernt) * (1.0 - (dt*dt)) :: Float

		refracted = ((uv - (n *. dt)) *. niOvernt) - (n *. sqrt discriminant) :: Vec3
