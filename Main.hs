import Data.List (intercalate)
import Utils.Vector.Vec3
import Utils.Geo.Ray
import Utils.Geo.Sphere
import System.IO

type Pixel = Int
newtype RGB = RGB (Int, Int, Int) deriving Show
type Scene = [RGB]

data Dimension = Dimension {
    width :: Pixel,
    height :: Pixel
} deriving Show

sceneWidth :: Pixel
sceneWidth = 200 

sceneHeight :: Pixel
sceneHeight = 100

dimension :: Dimension
dimension = Dimension sceneWidth sceneHeight

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
           | t >= 0.0 = ivec3 (x n + 1) (y n + 1) (z n + 1) *. 0.5
           | even (i + j) = ivec3 0.0 0.0 1.0
           | otherwise = smoothStep
           where
               sc = ivec3 0.0 0.0 (-1.0)
               (t, n) = hitSphere (Sphere 0.5 sc) ray

               unit_direction = makeUnitVector $ direction ray
               lerp1 = (ivec3 0.0 0.0 1.0 *. u) + (ivec3 0.0 1.0 0.0 *. (1.0 - u))
               lerp2 = (ivec3 0.0 0.0 0.0 *. u) + (ivec3 1.0 0.0 1.0 *. (1.0 - u))
               smoothStep = (lerp1 *. v) + (lerp2 *. (1.0 - v))

generateScene :: Scene
generateScene =
     [value i j | i <- [0 .. height dimension - 1],  j <- [0 .. width dimension - 1]]
     where
       value i j = RGB (ir, ig, ib)
         where
             u = fromIntegral j / fromIntegral (width dimension)
             v = fromIntegral i / fromIntegral (height dimension)
             origin = ivec3 0.0 0.0 0.0
             lower_left_corner = ivec3 (-2.0) (-1.0) (-1.0)
             horizontal = ivec3 4.0 0.0 0.0
             vertical = ivec3 0.0 2.0 0.0
             r = iray origin $ lower_left_corner + (horizontal *. u) + (vertical *. v)
             col = getColor r i j u v
             ir = floor $ 255 * x col
             ig = floor $ 255 * y col
             ib = floor $ 255 * z col

main :: IO ()
main = 
    writeAsPPM generateScene
