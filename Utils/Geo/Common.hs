module Utils.Geo.Common where

import Utils.Geo.Hittable (HitData (..), iHitData)
import Utils.Geo.Ray (Ray)
import Utils.Geo.Sphere
import Utils.Vector.Vec3

newtype Object = SphereObject Sphere

hitObject :: Ray -> Object -> HitData
hitObject ray (SphereObject sphere) = hit sphere ray

hitDoesIt :: Ray -> [Object] -> HitData
hitDoesIt ray scene
    | all (\i -> t i == -1.0) hits = notValidHit
    | otherwise = foldr closestHit noHit hits
  where
    hits = map (hitObject ray) scene

    closestHit i j
        | t i == -1.0 = j
        | t j == -1.0 = i
        | t i < t j = i
        | otherwise = j

    noHit = HitData (1.0 / 0.0) (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
    notValidHit = HitData (-1.0) (ivec3 0.0 0.0 0.0) (ivec3 0.0 0.0 0.0)
