module Utils.Geo.Sphere (
    Sphere (..),
    isphereWithRad,
    isphereWithPos,
    isphere,
    isphereDefault,
    hitSphere,
) where

import Utils.Vector.Vec3
import Utils.Geo.Ray

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

hitSphere :: Sphere -> Ray -> (Float, Vec3) 
hitSphere s r
   | x >= 0           = (t - x, np)
   | otherwise        = (-1.0, ivec3 0.0 0.0 0.0)
   where
        t = dot (position s - origin r) (direction r)
        p = pointAtT r t
        y = vectorLength $ position s - p
        rad = radius s
        x = sqrt $ rad * rad - y * y
        normalVector = pointAtT r (t - x) - position s
        np = makeUnitVector normalVector
