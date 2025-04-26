import Data.List (intercalate)
import Utils.Vector.Vec3
import Utils.Geo.Ray
import Utils.Geo.Sphere

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

writeAsPMM :: Scene -> IO ()
writeAsPMM scene = writeFile outputPath $
                             "P3\n"
                             ++ dimension_string
                             ++ "\n255\n" 
                             ++ intercalate "\n" (map value scene)
                                where
                                    value (RGB (a, b, c)) = unwords (map show [a, b, c])
                                    dimension_string = unwords (map show [width dimension, height dimension])


hitSphere :: Sphere -> Ray -> Bool 
hitSphere s r = rad - y * y > 0
                where
                    t = dot (position s - origin r) (direction r)
                    p = pointAtT r t
                    y = vectorLength (position s - p)
                    rad = radius s ** radius s

getColor :: Ray -> Int -> Int -> Float -> Float -> Vec3
getColor ray i j u v
           | hitSphere (Sphere 0.5 $ ivec3 0.0 0.0 1.0) ray = ivec3 1.0 0.0 0.0
           | otherwise = smoothStep
           where
               unit_direction = makeUnitVector $ direction ray
               lerp1 = (ivec3 0.0 0.0 1.0 *. u) + (ivec3 0.0 1.0 0.0 *. (1.0 - u))
               lerp2 = (ivec3 0.0 0.0 0.0 *. u) + (ivec3 1.0 0.0 1.0 *. (1.0 - u))
               smoothStep = (lerp1 *. v) + (lerp2 *. (1.0 - v))
               t = 0.5 * (y unit_direction + 1.0)

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
    writeAsPMM generateScene
