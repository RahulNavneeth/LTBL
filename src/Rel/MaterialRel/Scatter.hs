module Rel.MaterialRel.Scatter where

import Rel.MaterialRel.Base (Scatterable (..))
import Rel.MaterialRel.Lambertian (lambertianScatter)
import SceneDescriptor.Attribute.Material.Lambertian (Lambertian)

-- Might change how this works - just a bare function would do
instance Scatterable a where
	scatter (Lambertian material) hitData = lambertianScatter material hitData
