module Rel.MeshRel.IHit.Hittable (Object (..), hitDoesIt) where

import qualified Data.Vector as V
import Rel.MeshRel.IHit.Base (HitData (..), iHitData)
import Struct.Ray (Ray)
import Rel.MeshRel.Sphere.Hit
import Rel.MeshRel.Box.Hit
import Struct.Vector.Vec3

import SceneDescriptor.Attribute.Mesh.Primitive.Sphere (Sphere)

data Object = SphereObject Sphere | BoxObject Box

hitObject :: Ray -> Object -> HitData
hitObject ray (SphereObject sphere) = hit sphere ray
hitObject ray (BoxObject box) = hit box ray

hitDoesIt :: Ray -> V.Vector Object -> HitData
hitDoesIt ray world = V.foldl' closestHit notValidHit hits
  where
    hits = V.map (hitObject ray) world

    closestHit acc h
      | t h == -1.0 = acc
      | t acc == -1.0 = h
      | t h < t acc = h
      | otherwise = acc

    notValidHit = iHitData (-1.0) (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
