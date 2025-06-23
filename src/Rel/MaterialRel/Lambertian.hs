module Rel.MaterialRel.Lambertian (lambertianScatter) where

import Cbits.Interface.DRand (randomInUnitSphere)
import Struct.Vector.Vec3 (ivec3)
import Struct.Ray (iray)
import Rel.MeshRel.IHit.Base (HitData (..))
import SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation)

lambertianScatter :: Material -> HitData -> IO (Scatter, Attenuation)
lambertianScatter material hitData = do
	ran <- randomInUnitSphere
	let target = (p hitData) + (normal hitData)
	return (iray (p hitData) (target - p hitData), albedo material)
