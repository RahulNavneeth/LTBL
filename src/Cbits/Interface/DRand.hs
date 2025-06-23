{-# LANGUAGE ForeignFunctionInterface #-}

module Cbits.Interface.DRand (
	dRand,
	randomInUnitSphere
) where

import Foreign
import Foreign.C.Types
import Struct.Vector.Vec3

foreign import ccall unsafe "stdlib.h drand48" c_drand :: IO CDouble

dRand :: IO Float
dRand = fmap realToFrac c_drand

randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  x <- dRand
  y <- dRand
  z <- dRand
  let v = (ivec3 x y z *. 2.0) - ivec3 1.0 1.0 1.0
  if squaredLength v >= 1.0
    then randomInUnitSphere
    else return v
