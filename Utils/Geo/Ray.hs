module Utils.Geo.Ray (
    Ray (..),
    iray,
    origin,
    direction,
    pointAtT,
) where

import Utils.Vector.Vec3

newtype Ray = Ray {elements :: [Vec3]} deriving (Show, Eq)

iray :: Vec3 -> Vec3 -> Ray
iray a b = Ray [a, b]

origin :: Ray -> Vec3
origin (Ray r) = head r

direction :: Ray -> Vec3
direction (Ray r) = r !! 1

pointAtT :: Ray -> Float -> Vec3
pointAtT r t = origin r + direction r *. t
