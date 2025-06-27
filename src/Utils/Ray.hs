module Utils.Ray (
	reflect,
	refract,
	schlick
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
	
schlick :: Float -> Float -> Float 
schlick cosine refractiveIndex = r1 + (1.0-r1)*((1-cosine)**5)
  where
    r0 = (1-refractiveIndex)/(1+refractiveIndex)
    r1 = r0*r0
