module Rel.MeshRel.IHit.Base (
	HitData (..),
    Hittable (hit),
    iHitData,
) where

import Struct.Ray
import Struct.Vector.Vec3

data HitData = HitData {
	t :: Float,
    p :: Vec3,
    normal :: Vec3
} deriving (Show)

iHitData :: Float -> Vec3 -> Vec3 -> HitData
iHitData = HitData

class Hittable a where
  hit :: a -> Ray -> HitData
