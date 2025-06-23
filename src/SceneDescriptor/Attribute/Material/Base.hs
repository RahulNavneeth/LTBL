module SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation) where

import Struct.Vector.Vec3
import Struct.Ray

type Scatter = Ray
type Attenuation = Vec3

data Material = Material { 
	albedo :: Vec3
} deriving Show
