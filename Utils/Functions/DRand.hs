module Utils.Functions.DRand (
    initSeed,
    generateRandoms,
    drand48,
) where

import Data.Bits (xor)

a, c, m :: Int
a = 0x5DEECE66D
c = 0xB
m = 0x1000000000000

initSeed :: Int -> Int
initSeed seed = (seed `xor` a) `mod` m

next :: Int -> (Float, Int)
next seed = (fromIntegral nextInt / fromIntegral m, nextSeed)
  where
    nextSeed = (a * seed + c) `mod` m
    nextInt = nextSeed

drand48 :: Int -> (Float, Int)
drand48 = next

generateRandoms :: Int -> Int -> [Float]
generateRandoms _ 0 = []
generateRandoms seed n = value : generateRandoms newSeed (n - 1)
  where
    (value, newSeed) = drand48 seed
