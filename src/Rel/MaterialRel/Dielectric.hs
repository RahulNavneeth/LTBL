module Rel.MaterialRel.Dielectric (dielectricScatter) where

import Struct.Vector.Vec3
import Data.Maybe
import Utils.Ray
import Struct.Ray
import Cbits.Interface.DRand
import Rel.MeshRel.IHit.Base (HitData (..))
import Rel.MaterialRel.Base (ScatterData (..))
import SceneDescriptor.Attribute.Material.Dielectric (Dielectric (..))

dielectricScatter :: Dielectric -> Ray -> HitData -> IO ScatterData
dielectricScatter material ray hitData = do
  let attenuation = ivec3 1.0 1.0 1.0
      (outwardNormal, niOverNt, cosine) =
        if dot (direction ray) (normal hitData) > 0.0
          then (
            (normal hitData) *. (-1),
            refractiveIndex material,
            (refractiveIndex material * dot (direction ray) (normal hitData)) / vectorLength (direction ray)
          )
          else (
            normal hitData,
            1.0 / refractiveIndex material,
            (-1.0 * dot (direction ray) (normal hitData)) / vectorLength (direction ray)
          )

  let refracted = refract (direction ray) outwardNormal niOverNt
  let reflectProb =
        if isJust refracted
          then schlick cosine (refractiveIndex material)
          else 1.0

  ran <- dRand
  if ran < reflectProb
    then return $ ScatterData True (iray (p hitData) (reflect (direction ray) (normal hitData))) attenuation
    else return $ ScatterData True (iray (p hitData) (fromJust refracted)) attenuation
