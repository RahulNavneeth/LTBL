{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import LTBL 
import System.IO

type Pixel = Int
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

totalDepth :: Int
totalDepth = 50

dimension :: Dimension
dimension = Dimension sceneWidth sceneHeight sceneSamples

outputPath :: FilePath
outputPath = "./scene.ppm"

getIndex :: Int -> Int -> Int
getIndex i j = i * width dimension + j

world :: V.Vector Attribute
world =
  V.fromList
    [
	  Attribute 
		(SphereAttribute (isphere 100.0 (ivec3 0.0 (-100.5) (-1.0))))
		(LambertianMaterial (ilambertian (ivec3 0.8 0.8 0.0)))
		(RGB (ivec3 0.26 0.54 0.355)),
      Attribute 
	  	(SphereAttribute (isphere 0.5 (ivec3 0.0 0.0 (-1.0))))
	  	(LambertianMaterial (ilambertian (ivec3 0.1 0.2 0.5)))
		(RGB (ivec3 0.03 0.0 1)),
      Attribute
	  	(SphereAttribute (isphere 0.5 (ivec3 1.0 0.0 (-1.0))))
	  	(MetalMaterial (imetal (ivec3 0.8 0.6 0.2) 0.2))
		(RGB (ivec3 0.85 1.0 0.36)),
      Attribute
	  	(SphereAttribute (isphere 0.5 (ivec3 (-1.0) 0.0 (-1.0))))
	  	(DielectricMaterial (idielectric 1.5))
		(RGB (ivec3 1.0 0.83 0.96)),
      Attribute
	  	(SphereAttribute (isphere (-0.45) (ivec3 (-1.0) 0.0 (-1.0))))
	  	(DielectricMaterial (idielectric 1.5))
		(RGB (ivec3 1.0 0.83 0.96))
    ]

writeAsPPM :: Scene -> IO ()
writeAsPPM scene = withFile "scene.ppm" WriteMode $ \handle -> do
  hPutStrLn handle "P3"
  hPutStrLn handle (unwords [show (width dimension), show (height dimension)])
  hPutStrLn handle "255"
  mapM_ (hPutStrLn handle . value) scene
  where
    value (RGB v) = unwords $ map (show . toInt) [r v, g v, b v]
    toInt x = floor (clamp x)
    clamp x = max 0 (min 255 x)

getScenePixel :: Ray -> Int -> Int -> Float -> Float -> Int -> IO Vec3
getScenePixel ray i j u v depth = do
  let unit_direction = makeUnitVector $ direction ray
      t' = 0.5 * (y unit_direction + 1.0)
      sky = ivec3 1.0 1.0 1.0 *. (1 - t') + ivec3 0.5 0.7 1.0 *. t'
  if depth <= 0 then
    return sky
  else do
    case hitDoesIt ray world of
      Just (xs, attribute) -> do
        scatterData <- scatter (material attribute) ray xs
        if validScatter scatterData
          then do
            color <- getScenePixel (scatteredRay scatterData) i j u v (depth - 1)
            return (color * (getRGB (diffuseColor attribute)))
            -- return (color * (attenuation scatterData))
          else return $ ivec3 0.0 0.0 0.0
      Nothing -> do
        -- if even (i + j)
        --   then return (ivec3 0.0 0.0 1.0)
        -- else return sky
        return sky


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
      let r' = 255.9 * sqrt (x col)
          g' = 255.9 * sqrt (y col)
          b' = 255.9 * sqrt (z col)
      
      return $ RGB (ivec3 r' g' b')
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
                c <- getScenePixel r i j u v totalDepth
                go (acc + c) (s + 1)

main :: IO ()
main = initScene >>= \scene -> writeAsPPM scene
