module SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation) where

import Struct.Vector.Vec3
import SceneDescriptor.Attribute.Material.Lambertian
import Struct.Ray

type Scatter = Ray
type Attenuation = Vec3

data Material = LambertianMaterial Lambertian deriving Show
