module Utils.Geo.Common where

import Utils.Geo.Hittable (HitData (..))
import Utils.Geo.Sphere
import Utils.Geo.Ray (Ray)

newtype Object = SphereObject Sphere

hitObject :: Ray -> Object -> HitData
hitObject ray (SphereObject sphere) = hit sphere ray

hitDoesIt :: Ray -> [Object] -> [HitData]
hitDoesIt ray scene = filter (\hit -> t hit >= 0.0) $ map (hitObject ray) scene
