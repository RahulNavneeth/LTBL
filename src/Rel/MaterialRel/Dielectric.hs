module Rel.MaterialRel.Dielectric (dielectricScatter) where

import Struct.Vector.Vec3
import Utils.Ray
import Struct.Ray
import Rel.MeshRel.IHit.Base (HitData (..))
import Rel.MaterialRel.Base (ScatterData (..))
import SceneDescriptor.Attribute.Material.Dielectric (Dielectric (..))

dielectricScatter :: Dielectric -> Ray -> HitData -> IO ScatterData
dielectricScatter material ray hitData = do
  let attenuation = ivec3 1.0 1.0 1.0
      (outwardNormal, niOverNt) =
        if dot (direction ray) (normal hitData) > 0.0
        then ((normal hitData) *. (-1), refractiveIndex material)
        else (normal hitData, 1.0 / refractiveIndex material)
  case refract (direction ray) outwardNormal niOverNt of
    Just refracted -> return $ ScatterData True (iray (p hitData) refracted) attenuation
    Nothing        -> return $ ScatterData False (iray (p hitData) (reflect (direction ray) (normal hitData))) attenuation
