module Rel.MaterialRel.Metal (metalScatter) where

import Struct.Vector.Vec3
import Struct.Ray
import Rel.MeshRel.IHit.Base (HitData (..))
import SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation)
import SceneDescriptor.Attribute.Material.Metal (Metal (..))

-- Move this function to a utils folder
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n *. (2.0*(dot v n)))

metalScatter :: Metal -> Ray -> HitData -> IO (Scatter, Attenuation)
metalScatter material ray hitData = do
	let reflected = reflect (direction ray) (normal hitData)
	return (iray (p hitData) reflected, albedo material)
