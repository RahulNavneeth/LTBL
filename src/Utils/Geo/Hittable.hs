module Utils.Geo.Hittable
  ( HitData (..),
    iHitData,
    Hittable (hit),
  )
where

import Utils.Geo.Ray
import Utils.Vector.Vec3

data HitData = HitData
  { t :: Float,
    p :: Vec3,
    normal :: Vec3
  }

iHitData :: Float -> Vec3 -> Vec3 -> HitData
iHitData = HitData

class Hittable a where
  hit :: a -> Ray -> HitData
