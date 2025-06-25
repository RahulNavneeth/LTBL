module Rel.MaterialRel.Metal (metalScatter) where

import Struct.Vector.Vec3
import Struct.Ray
import Rel.MeshRel.IHit.Base (HitData (..))
import Rel.MaterialRel.Base (ScatterData (..))
import SceneDescriptor.Attribute.Material.Metal (Metal (..))
import Cbits.Interface.DRand

-- Move this function to a utils folder
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n *. (2.0 * dot v n))

metalScatter :: Metal -> Ray -> HitData -> IO ScatterData
metalScatter material ray hitData = do
  ran <- randomInUnitSphere
  let reflected = reflect (direction ray) (normal hitData)
      scatter = iray (p hitData) (reflected + ran *. fuzz material)
      validScatter = dot (direction scatter) (normal hitData) > 0.0
  return $ ScatterData validScatter scatter (albedo material)
