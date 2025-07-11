module Rel.MaterialRel.Scatter where

import Struct.Ray
import Rel.MaterialRel.Base (Scatterable (..))
import Rel.MaterialRel.Lambertian (lambertianScatter)
import Rel.MaterialRel.Metal (metalScatter)
import Rel.MaterialRel.Dielectric (dielectricScatter)
import SceneDescriptor.Attribute.Material.Base (Material (..))

instance Scatterable Material where
	scatter (LambertianMaterial material) ray hitData = lambertianScatter material ray hitData
	scatter (MetalMaterial material) ray hitData = metalScatter material ray hitData
	scatter (DielectricMaterial material) ray hitData = dielectricScatter material ray hitData
