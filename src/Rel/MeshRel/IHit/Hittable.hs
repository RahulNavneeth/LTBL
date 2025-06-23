module Rel.MeshRel.IHit.Hittable (hitDoesIt) where

import qualified Data.Vector as V
import Rel.MeshRel.IHit.Base (HitData (..), Hittable (..), iHitData)
import Struct.Ray (Ray)
import SceneDescriptor.Attribute.Mesh.Base (Mesh (..))
import SceneDescriptor.Attribute.Base (Attribute (..))
import Rel.MeshRel.Sphere.Hit
import Rel.MeshRel.Box.Hit
import Struct.Vector.Vec3

instance Hittable Mesh where
	hit (SphereAttribute sphere) ray = hitSphere sphere ray
	hit (BoxAttribute box) ray = hitBox box ray

hitObject :: Ray -> Attribute -> (HitData, Attribute)
hitObject ray attribute = (hit (mesh attribute) ray, attribute)

hitDoesIt :: Ray -> V.Vector Attribute -> HitData
hitDoesIt ray world = V.foldl' closestHit notValidHit hits
  where
    hits = V.map (hitObject ray) world

    closestHit acc (h, _)
      | t h == -1.0 = acc
      | t acc == -1.0 = h
      | t h < t acc = h
      | otherwise = acc

    notValidHit = iHitData (-1.0) (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
