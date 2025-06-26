module SceneDescriptor.Attribute.Material.Dielectric (
	Dielectric (..),
	idielectric,
	idielectricDefault,
	RefractiveIndex
) where

type RefractiveIndex = Float

data Dielectric =  Dielectric {
	refractiveIndex :: RefractiveIndex
} deriving Show

idielectric :: Float -> Dielectric
idielectric = Dielectric

idielectricDefault :: Dielectric
idielectricDefault = Dielectric 1.5
