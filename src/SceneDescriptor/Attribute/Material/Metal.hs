module SceneDescriptor.Attribute.Material.Metal (Metal (..), imetal) where

import Struct.Vector.Vec3

data Metal = Metal {
	albedo :: Vec3
} deriving Show

imetal :: Vec3 -> Metal
imetal = Metal
