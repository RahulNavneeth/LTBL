module SceneDescriptor.Attribute.Material.Lambertian (Lambertian (..), iLambertian) where

import SceneDescriptor.Attribute.Material.Base (Material (..))

data Lambertian = Lambertian Material

iLambertian :: Lambertian
iLambertian albedo = Lambertian $ Material albedo
