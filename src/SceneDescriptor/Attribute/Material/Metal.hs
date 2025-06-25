module SceneDescriptor.Attribute.Material.Metal (
	Metal (..),
	imetal,
	imetalWithoutFuzz,
) where

import Struct.Vector.Vec3

data Metal = Metal {
	albedo :: Vec3,
	fuzz :: Float
} deriving Show

imetal :: Vec3 -> Float -> Metal
imetal albedo fuzz = Metal albedo $ min fuzz 1.0

imetalWithoutFuzz :: Vec3 -> Metal
imetalWithoutFuzz albedo = Metal albedo 0.0
