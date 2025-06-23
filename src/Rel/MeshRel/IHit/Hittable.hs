module Rel.MeshRel.IHit.Hittable (hitDoesIt) where

import qualified Data.Vector as V
import Rel.MeshRel.IHit.Base (HitData (..), Hittable (..), iHitData)
import Struct.Ray (Ray)
import SceneDescriptor.Attribute.Mesh.Base (Mesh (..))
import SceneDescriptor.Attribute.Base (Attribute (..))
import Rel.MeshRel.Sphere.Hit
import Rel.MeshRel.Box.Hit
import Struct.Vector.Vec3
import Data.Maybe

instance Hittable Mesh where
	hit (SphereAttribute sphere) ray = hitSphere sphere ray
	hit (BoxAttribute box) ray = hitBox box ray

hitObject :: Ray -> Attribute -> (HitData, Attribute)
hitObject ray attribute = (hit (mesh attribute) ray, attribute)

hitDoesIt :: Ray -> V.Vector Attribute -> Maybe (HitData, Attribute)
hitDoesIt ray world = V.foldl' closestHit notValidHit hits
  where
    hits = V.map (hitObject ray) world

    closestHit Nothing (h, material)
      | t h == -1.0 = Nothing
      | otherwise = Just (h, material)
    closestHit (Just (acc, mat)) (h, material)
      | t h == -1.0 = Just (acc, mat)
      | t h < t acc = Just (h, material)
      | otherwise = Just (acc, mat)

    notValidHit = Nothing
