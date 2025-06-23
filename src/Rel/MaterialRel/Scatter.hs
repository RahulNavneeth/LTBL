module Rel.MaterialRel.Scatter where

import Rel.MaterialRel.Base (Scatterable (..))
import Rel.MaterialRel.Lambertian (lambertianScatter)
import SceneDescriptor.Attribute.Material.Base (Material (..))

-- Might change how this works - just a bare function would do
instance Scatterable Material where
	scatter (LambertianMaterial material) hitData = lambertianScatter material hitData
