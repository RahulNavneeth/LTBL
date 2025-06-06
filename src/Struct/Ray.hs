module Struct.Ray (
	Ray (..),
    iray,
    origin,
    direction,
    pointAtT,
) where

import qualified Data.Vector as V
import Struct.Vector.Vec3

newtype Ray = Ray {ray_elements :: V.Vector Vec3} deriving (Show, Eq)

iray :: Vec3 -> Vec3 -> Ray
iray a b = Ray $ V.fromList [a, b]

origin :: Ray -> Vec3
origin (Ray r) = r V.! 0

direction :: Ray -> Vec3
direction (Ray r) = r V.! 1

pointAtT :: Ray -> Float -> Vec3
pointAtT r t = origin r + direction r *. t
