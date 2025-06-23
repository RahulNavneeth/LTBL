module SceneDescriptor.Attribute.Base (Attribute (..)) where

import SceneDescriptor.Attribute.Mesh.Base (Mesh)
import SceneDescriptor.Attribute.Material.Base (Material)

data Attribute = Attribute {
	mesh :: Mesh,
	material :: Material
} deriving Show
