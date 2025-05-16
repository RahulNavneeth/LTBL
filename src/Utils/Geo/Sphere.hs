module Utils.Geo.Sphere
  ( Sphere (..),
    Hittable (hit),
    isphereWithRad,
    isphereWithPos,
    isphere,
    isphereDefault,
  )
where

import Utils.Geo.Hittable
import Utils.Geo.Ray
import Utils.Vector.Vec3

data Sphere = Sphere
  { radius :: Float,
    position :: Vec3
  }
  deriving (Show)

isphereWithRad :: Float -> Sphere
isphereWithRad rad = Sphere rad (ivec3 0.0 0.0 0.0)

isphereWithPos :: Vec3 -> Sphere
isphereWithPos = Sphere 1.0

isphere :: Float -> Vec3 -> Sphere
isphere = Sphere

isphereDefault :: Sphere
isphereDefault = Sphere 1.0 (ivec3 0.0 0.0 0.0)

instance Hittable Sphere where
  hit :: Sphere -> Ray -> HitData
  hit s r
    | discriminant < 0 = iHitData (-1.0) (ivec3 0 0 0) (ivec3 0 0 0)
    | firstRoot > 0 = iHitData firstRoot hitPoint normalVector
    | secondRoot > 0 = iHitData secondRoot hitPoint2 normalVector2
    | otherwise = iHitData (-1.0) (ivec3 0 0 0) (ivec3 0 0 0)
    where
      oc = origin r - position s
      a = dot (direction r) (direction r)
      b = 2.0 * dot oc (direction r)
      c = dot oc oc - radius s * radius s
      discriminant = b * b - 4 * a * c

      firstRoot = (-b - sqrt discriminant) / (2 * a)
      secondRoot = (-b + sqrt discriminant) / (2 * a)

      hitPoint = pointAtT r firstRoot
      normalVector = makeUnitVector (hitPoint - position s)

      hitPoint2 = pointAtT r secondRoot
      normalVector2 = makeUnitVector (hitPoint2 - position s)
