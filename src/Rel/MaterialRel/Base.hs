module Rel.MaterialRel.Base (Scatterable (..), ScatterData (..)) where

import Struct.Ray
import Struct.Vector.Vec3
import Rel.MeshRel.IHit.Base
import SceneDescriptor.Attribute.Material.Base (Scatter, Attenuation)

data ScatterData = ScatterData {
	validScatter :: Bool,
	scatteredRay :: Scatter,
	attenuation :: Attenuation
} deriving Show

-- Change the name 'Scatterable'
class Scatterable a where
	scatter :: a -> Ray -> HitData -> IO (ScatterData)
