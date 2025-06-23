module Rel.MeshRel.Box.Hit ( hitBox ) where

import SceneDescriptor.Attribute.Mesh.Primitive.Box (Box)
import Rel.MeshRel.IHit.Base (HitData, iHitData)
import SceneDescriptor.Attribute.Mesh.Base (Mesh (..))
import Struct.Ray (Ray)
import Struct.Vector.Vec3

hitBox :: Box -> Ray -> HitData
hitBox box ray = iHitData 0.0 (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
