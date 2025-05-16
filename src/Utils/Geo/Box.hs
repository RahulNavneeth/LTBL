module Utils.Geo.Box
  ( Box (..),
    Hittable (hit),
    iBox,
  )
where

import Utils.Geo.Hittable (HitData, Hittable (hit), iHitData)
import Utils.Geo.Ray (Ray)
import Utils.Vector.Vec3

data Box = Box
  { vmin :: Vec3,
    vmax :: Vec3
  }
  deriving (Show)

iBox :: Vec3 -> Vec3 -> Box
iBox = Box

instance Hittable Box where
  hit :: Box -> Ray -> HitData
  hit box ray = iHitData 0.0 (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
