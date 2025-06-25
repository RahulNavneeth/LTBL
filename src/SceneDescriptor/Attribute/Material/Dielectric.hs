module SceneDescriptor.Attribute.Material.Dielectric (
	Dielectric (..),
	idielectric,
	idielectricDefault
) where

data Dielectric =  Dielectric {
	refractiveIndex :: Float
} deriving Show

idielectric :: Float -> Dielectric
idielectric = Dielectric

idielectricDefault :: Dielectric
idielectricDefault = Dielectric 1.5
