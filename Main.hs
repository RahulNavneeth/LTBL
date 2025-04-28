import Utils.Functions.DRand
import Utils.Geo.Common
import Utils.Geo.Ray
import Utils.Geo.Sphere
import Utils.Scene.Camera
import Utils.Vector.Vec3

-- TODO: Make sure not to import everything everytime
import Utils.Geo.Hittable (HitData (..))

import Data.List (intercalate)
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
sceneSamples = 4

dimension :: Dimension
dimension = Dimension sceneWidth sceneHeight sceneSamples

outputPath :: FilePath
outputPath = "./scene.ppm"

getIndex :: Int -> Int -> Int
getIndex i j = i * width dimension + j

writeAsPPM :: Scene -> IO ()
writeAsPPM scene = withFile outputPath WriteMode $ \handle -> do
    hPutStrLn handle "P3"
    hPutStrLn handle (unwords [show $ width dimension, show $ height dimension])
    hPutStrLn handle "255"
    mapM_ (hPutStrLn handle . value) scene
  where
    value (RGB (r, g, b)) = unwords (map show [r, g, b])

getColor :: Ray -> Int -> Int -> Float -> Float -> Vec3
getColor ray i j u v
    | t xs > 0.0 = ivec3 (x n + 1) (y n + 1) (z n + 1) *. 0.5
    | even (i + j) = ivec3 0.0 0.0 1.0
    | otherwise = smoothStep
  where
    sc = ivec3 0.0 0.0 (-1.0)
    xs =
        hitDoesIt
            ray
            [ SphereObject $ Sphere 100.0 (ivec3 0.0 (-100.5) (-1.0))
            , SphereObject $ Sphere 0.5 (ivec3 0.0 0.0 (-1.0))
            , SphereObject $ Sphere 0.5 (ivec3 (-0.8) 0.0 (-1.4))
            ]
    n = normal xs

    unit_direction = makeUnitVector $ direction ray
    lerp1 = (ivec3 0.0 0.0 1.0 *. u) + (ivec3 0.0 1.0 0.0 *. (1.0 - u))
    lerp2 = (ivec3 0.0 0.0 0.0 *. u) + (ivec3 1.0 0.0 1.0 *. (1.0 - u))
    smoothStep = (lerp1 *. v) + (lerp2 *. (1.0 - v))

drandValue :: [Float]
drandValue = generateRandoms 12345 $ width dimension * height dimension * samples dimension * 2

generateScene :: Scene
generateScene =
    [value i j |
        i <- [height dimension - 1, height dimension - 2 .. 0],
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

        col =
            foldr
                (+)
                (ivec3 0.0 0.0 0.0)
                [sampleColor s| s <- [0 .. samples dimension - 1]]
                /. fromIntegral (samples dimension)

        sampleColor s = 
          let
            baseIdx = getIndex i j * 2 * samples dimension
            uRand = drandValue !! (baseIdx + s * 2)
            vRand = drandValue !! (baseIdx + s * 2 + 1)
            
            u = (fromIntegral j + uRand) / fromIntegral (width dimension)
            v = (fromIntegral i + vRand) / fromIntegral (height dimension)
            
            r = getRay cam u v
          in
            getColor r i j u v

        ir = floor $ 255.9 * x col
        ig = floor $ 255.9 * y col
        ib = floor $ 255.9 * z col

main :: IO ()
main =
    writeAsPPM generateScene
