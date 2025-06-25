module SceneDescriptor.Attribute.Material.Base (Material (..), Scatter, Attenuation) where

import Struct.Ray
import Struct.Vector.Vec3
import SceneDescriptor.Attribute.Material.Metal
import SceneDescriptor.Attribute.Material.Dielectric
import SceneDescriptor.Attribute.Material.Lambertian

type Scatter = Ray
type Attenuation = Vec3

data Material = LambertianMaterial Lambertian |
				MetalMaterial Metal |
				DielectricMaterial Dielectric deriving Show
