module Utils.Geo.Sphere (
    Sphere (..),
    isphereWithRad,
    isphereWithPos,
    isphere,
    isphereDefault,
) where

import Utils.Vector.Vec3

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
