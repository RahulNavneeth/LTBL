module Utils.Functions.DRand
  ( initSeed,
    drand48,
    generateRandoms,
    randomInUnitSphere,
  )
where

import Data.Bits (xor)
import Data.Vector.Unboxed as VU
import Utils.Vector.Vec3

a, c, m :: Integer
a = 0x5DEECE66D
c = 0xB
m = 2 ^ 48

initSeed :: Integer -> Integer
initSeed seed = (seed `xor` a) `mod` m

next :: Integer -> (Float, Integer)
next seed = (fromIntegral nextSeed / fromIntegral m, nextSeed)
  where
    nextSeed = (a * seed + c) `mod` m

drand48 :: Integer -> (Float, Integer)
drand48 = next

generateRandoms :: Integer -> Int -> VU.Vector Float
generateRandoms _ 0 = VU.empty
generateRandoms seed n = VU.cons value $ generateRandoms newSeed (n - 1)
  where
    (value, newSeed) = drand48 seed

randomInUnitSphere :: Integer -> (Vec3, Integer)
randomInUnitSphere seed
  | squaredLength v >= 1.0 = randomInUnitSphere seed'''
  | otherwise = (v, seed''')
  where
    (x, seed') = drand48 seed
    (y, seed'') = drand48 seed'
    (z, seed''') = drand48 seed''
    v = (ivec3 x y z *. 2.0) - ivec3 1.0 1.0 1.0
