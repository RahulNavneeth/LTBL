module SceneDescriptor.Attribute.Mesh.Base (Mesh (..)) where

import SceneDescriptor.Attribute.Mesh.Primitive.Sphere (Sphere)
import SceneDescriptor.Attribute.Mesh.Primitive.Box (Box)

data Mesh = SphereAttribute Sphere | BoxAttribute Box deriving Show
