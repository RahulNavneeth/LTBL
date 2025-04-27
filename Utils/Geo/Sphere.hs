module Utils.Geo.Sphere (
    Sphere (..),
    Hittable (hit),
    isphereWithRad,
    isphereWithPos,
    isphere,
    isphereDefault,
) where

import Utils.Vector.Vec3
import Utils.Geo.Ray
import Utils.Geo.Hittable

data Sphere = Sphere {
    radius :: Float,
    position :: Vec3
} deriving (Show)

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
       | x >= 0           = iHitData firstRoot newP normalVector
       | otherwise        = iHitData (-1.0) (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
       where
            t = dot (position s - origin r) (direction r)
            p = pointAtT r t
            y = vectorLength $ position s - p
            rad = radius s
            x = sqrt $ rad * rad - y * y
            -- Add 2nd root too?? (t + x)
            firstRoot = t - x
            newP = pointAtT r firstRoot
            normalPoint = newP - position s
            normalVector = makeUnitVector normalPoint
