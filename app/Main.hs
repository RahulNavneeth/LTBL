import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import LTBL
import System.IO

type Pixel = Int

newtype RGB = RGB (Int, Int, Int) deriving (Show)

type Scene = [RGB]

data Dimension = Dimension
  { width :: Pixel,
    height :: Pixel,
    samples :: Pixel
  }
  deriving (Show)

sceneWidth :: Pixel
sceneWidth = 200

sceneHeight :: Pixel
sceneHeight = 100

sceneSamples :: Pixel
sceneSamples = 10

dimension :: Dimension
dimension = Dimension sceneWidth sceneHeight sceneSamples

outputPath :: FilePath
outputPath = "./scene.ppm"

getIndex :: Int -> Int -> Int
getIndex i j = i * width dimension + j

world :: V.Vector Object
world =
  V.fromList
    [ SphereObject $ Sphere 100.0 (ivec3 0.0 (-100.5) (-1.0)),
      SphereObject $ Sphere 0.5 (ivec3 0.0 0.0 (-1.0))
      -- SphereObject $ Sphere 0.5 (ivec3 (-0.8) 0.0 (-1.4))
    ]

writeAsPPM :: Scene -> IO ()
writeAsPPM scene = withFile outputPath WriteMode $ \handle -> do
  hPutStrLn handle "P3"
  hPutStrLn handle (unwords [show $ width dimension, show $ height dimension])
  hPutStrLn handle "255"
  mapM_ (hPutStrLn handle . value) scene
  where
    value (RGB (r, g, b)) = unwords (map show [r, g, b])

getColor :: Ray -> Int -> Int -> Float -> Float -> Int -> Vec3
getColor ray i j u v depth
  -- \| depth < 0 = ivec3 1.0 0.0 0.0
  | t xs > 0.0 && depth > 0 = getColor (iray (p xs) (target - p xs)) i j u v (depth - 1) *. 0.5
  -- \| even (i + j) = ivec3 0.0 0.0 1.0
  | otherwise = sky
  where
    xs = hitDoesIt ray world
    s = randomInUnitSphere 12345
    target = p xs + normal xs + fst s

    unit_direction = makeUnitVector $ direction ray
    t' = 0.5 * (y unit_direction + 1.0)
    sky = ivec3 1.0 1.0 1.0 *. (1 - t') + ivec3 0.5 0.7 1.0 *. t'

-- lerp1 = (ivec3 0.0 0.0 0.0 *. u) + (ivec3 1.0 0.0 1.0 *. (1.0 - u))
-- lerp2 = (ivec3 0.0 0.0 1.0 *. u) + (ivec3 0.0 1.0 0.0 *. (1.0 - u))
-- smoothStep = (lerp1 *. v) + (lerp2 *. (1.0 - v))

drandValue :: VU.Vector Float
drandValue = generateRandoms 12345 $ width dimension * height dimension * samples dimension * 2

generateScene :: Scene
generateScene =
  [ value i j
    | i <- [height dimension - 1, height dimension - 2 .. 0],
      j <- [0 .. width dimension - 1]
  ]
  where
    value i j = RGB (ir, ig, ib)
      where
        origin = ivec3 0.0 0.0 0.0
        lower_left_corner = ivec3 (-2.0) (-1.0) (-1.0)
        horizontal = ivec3 4.0 0.0 0.0
        vertical = ivec3 0.0 2.0 0.0

        cam = icam origin lower_left_corner horizontal vertical

        nSamples = samples dimension
        baseIdx = getIndex i j * 2 * nSamples
        totalColor = go 0 0
          where
            go acc s
              | s == nSamples = acc
              | otherwise =
                  let uRand = drandValue VU.! (baseIdx + s * 2)
                      vRand = drandValue VU.! (baseIdx + s * 2 + 1)
                      u = (fromIntegral j + uRand) / fromIntegral (width dimension)
                      v = (fromIntegral i + vRand) / fromIntegral (height dimension)
                      r = getRay cam u v
                      c = getColor r i j u v 50
                   in go (acc + c) (s + 1)
        col = totalColor /. fromIntegral nSamples

        ir = floor $ 255.9 * x col
        ig = floor $ 255.9 * y col
        ib = floor $ 255.9 * z col

main :: IO ()
main = writeAsPPM generateScene
