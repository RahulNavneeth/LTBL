module SceneDescriptor.Attribute.Base (Attribute (..), RGB (..)) where

import Struct.Vector.Vec3
import SceneDescriptor.Attribute.Mesh.Base (Mesh)
import SceneDescriptor.Attribute.Material.Base (Material)

newtype RGB = RGB { getRGB :: Vec3  } deriving (Show)

data Attribute = Attribute {
	mesh :: Mesh,
	material :: Material,
	diffuseColor :: RGB
} deriving Show
