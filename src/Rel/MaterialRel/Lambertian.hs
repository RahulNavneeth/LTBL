module Rel.MaterialRel.Lambertian (lambertianScatter) where

import Cbits.Interface.DRand (randomInUnitSphere)
import Struct.Vector.Vec3 (ivec3)
import Struct.Ray
import Rel.MeshRel.IHit.Base (HitData (..))
import Rel.MaterialRel.Base (ScatterData (..))
import SceneDescriptor.Attribute.Material.Lambertian (Lambertian (..))

lambertianScatter :: Lambertian -> Ray -> HitData -> IO (ScatterData)
lambertianScatter material _ hitData = do
  ran <- randomInUnitSphere
  let target = (p hitData) + (normal hitData) + ran
      scatter = iray (p hitData) (target - p hitData)
  return $ ScatterData True scatter (albedo material)
