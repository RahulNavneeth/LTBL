{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import LTBL
import System.IO

type Pixel = Int

newtype RGB = RGB (Int, Int, Int) deriving (Show)

type Scene = [RGB]

data Dimension = Dimension {
	width :: Pixel,
    height :: Pixel,
    samples :: Pixel
} deriving (Show)

sceneWidth :: Pixel
sceneWidth = 200

sceneHeight :: Pixel
sceneHeight = 100

sceneSamples :: Pixel
sceneSamples = 100

dimension :: Dimension
dimension = Dimension sceneWidth sceneHeight sceneSamples

outputPath :: FilePath
outputPath = "./scene.ppm"

getIndex :: Int -> Int -> Int
getIndex i j = i * width dimension + j

world :: V.Vector Attribute
world =
  V.fromList
    [ Attribute (SphereAttribute $ Sphere 100.0 (ivec3 0.0 (-100.5) (-1.0))) Diffuse,
      Attribute (SphereAttribute $ Sphere 0.5 (ivec3 0.0 0.0 (-1.0))) Diffuse,
      Attribute (SphereAttribute $ Sphere 0.5 (ivec3 (-0.8) 0.0 (-1.4))) Diffuse
    ]

writeAsPPM :: Scene -> IO ()
writeAsPPM scene = withFile outputPath WriteMode $ \handle -> do
  hPutStrLn handle "P3"
  hPutStrLn handle (unwords [show $ width dimension, show $ height dimension])
  hPutStrLn handle "255"
  mapM_ (hPutStrLn handle . value) scene
  where
    value (RGB (r, g, b)) = unwords (map show [r, g, b])

getScenePixel :: Ray -> Int -> Int -> Float -> Float -> Int -> IO Vec3
getScenePixel ray i j u v depth = do
  let (xs, attribute) = hitDoesIt ray world
  	  s <- (material attribute)
      target = p xs + normal xs + s
      unit_direction = makeUnitVector $ direction ray
      t' = 0.5 * (y unit_direction + 1.0)
      sky = ivec3 1.0 1.0 1.0 *. (1 - t') + ivec3 0.5 0.7 1.0 *. t'
      -- lerp1 = (ivec3 0.0 0.0 0.0 *. u) + (ivec3 1.0 0.0 1.0 *. (1.0 - u))
      -- lerp2 = (ivec3 0.0 0.0 1.0 *. u) + (ivec3 0.0 1.0 0.0 *. (1.0 - u))
      -- smoothStep = (lerp1 *. v) + (lerp2 *. (1.0 - v))

  if t xs > 0.0 && depth > 0 then do
    color <- getScenePixel (iray (p xs) (target - p xs)) i j u v (depth - 1)
    return (color *. 0.5)
  -- else if even (i + j) then return (ivec3 0.0 0.0 1.0)
  else return sky

initScene :: IO Scene
initScene = do
  sequence
    [ value i j
      | i <- [height dimension - 1, height dimension - 2 .. 0],
        j <- [0 .. width dimension - 1]
    ]
  where
    value :: Int -> Int -> IO RGB
    value i j = do
      col <- go 0 0
      let ir = floor $ 255.9 * x col
          ig = floor $ 255.9 * y col
          ib = floor $ 255.9 * z col

      return $ RGB (ir, ig, ib)
      where
          go acc s
            | s == samples dimension = return (acc /. fromIntegral s)
            | otherwise = do
                uRand <- dRand
                vRand <- dRand
                let u = (fromIntegral j + uRand) / fromIntegral (width dimension)
                    v = (fromIntegral i + vRand) / fromIntegral (height dimension)
                    origin = ivec3 0.0 0.0 0.0
                    lower_left_corner = ivec3 (-2.0) (-1.0) (-1.0)
                    horizontal = ivec3 4.0 0.0 0.0
                    vertical = ivec3 0.0 2.0 0.0
                    cam = icam origin lower_left_corner horizontal vertical
                    r = getRay cam u v
                c <- getScenePixel r i j u v 50
                go (acc + c) (s + 1)

main :: IO ()
main = initScene >>= \scene -> writeAsPPM scene
