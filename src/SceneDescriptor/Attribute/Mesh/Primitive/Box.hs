module SceneDescriptor.Attribute.Mesh.Primitive.Box (Box (..), iBox) where

import Struct.Vector.Vec3

data Box = Box {
	vmin :: Vec3,
    vmax :: Vec3
} deriving (Show)

iBox :: Vec3 -> Vec3 -> Box
iBox = Box
