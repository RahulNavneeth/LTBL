module Rel.MaterialRel.Base (Scatterable (..)) where

import Struct.Ray
import Struct.Vector.Vec3
import Rel.MeshRel.IHit.Base
import SceneDescriptor.Attribute.Material.Base (Scatter, Attenuation)

-- Change the name 'Scatterable'
class Scatterable a where
	scatter :: a -> Ray -> HitData -> IO (Scatter, Attenuation)
