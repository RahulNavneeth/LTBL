module SceneDescriptor.Attribute.Mesh.Primitive.Sphere (
	Sphere (..),
	isphereWithRad,
	isphereWithPos,
	isphere,
	isphereDefault,
) where

import Struct.Vector.Vec3

{-
 - Sphere is one the primitive types that LTBL offers
 -
 - Parameters in order
 - @param {Float} radius
 - @param {Vec3} radius
 -
 -}
data Sphere = Sphere {
	radius :: Float,
	position :: Vec3
} deriving (Show, Eq)

isphereWithRad :: Float -> Sphere
isphereWithRad rad = Sphere rad (ivec3 0.0 0.0 0.0)

isphereWithPos :: Vec3 -> Sphere
isphereWithPos = Sphere 1.0

isphere :: Float -> Vec3 -> Sphere
isphere = Sphere

isphereDefault :: Sphere
isphereDefault = Sphere 1.0 (ivec3 0.0 0.0 0.0)

