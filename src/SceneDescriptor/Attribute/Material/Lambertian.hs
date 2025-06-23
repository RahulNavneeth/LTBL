module SceneDescriptor.Attribute.Material.Lambertian (Lambertian (..), ilambertian) where

import Struct.Vector.Vec3

data Lambertian = Lambertian {
	albedo :: Vec3
} deriving Show

ilambertian :: Vec3 -> Lambertian
ilambertian = Lambertian
