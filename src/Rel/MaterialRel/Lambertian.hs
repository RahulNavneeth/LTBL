module Rel.MaterialRel.Lambertian (lambertianScatter) where

import Cbits.Interface.DRand (randomInUnitSphere)
import Struct.Vector.Vec3 (ivec3)
import Struct.Ray
import Rel.MeshRel.IHit.Base (HitData (..))
import SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation)
import SceneDescriptor.Attribute.Material.Lambertian (Lambertian (..))

lambertianScatter :: Lambertian -> Ray -> HitData -> IO (Scatter, Attenuation)
lambertianScatter material _ hitData = do
	ran <- randomInUnitSphere
	let target = (p hitData) + (normal hitData) + ran
	return (iray (p hitData) (target - p hitData), albedo material)
