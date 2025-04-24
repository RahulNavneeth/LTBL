import Data.List (intercalate)

type Pixel = Int
newtype RGB = RGB (Int, Int, Int) deriving Show
type Scene = [RGB]

data Dimension = Dimension {
    width :: Pixel,
    height :: Pixel
} deriving Show

sceneWidth :: Pixel
sceneWidth = 1920 

sceneHeight :: Pixel
sceneHeight = 1080

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

generateScene :: Scene
generateScene =
     [value i j | i <- [0 .. height dimension - 1],  j <- [0 .. width dimension - 1]]
     where
       value i j = RGB (ir, ig, ib)
         where
           ir = floor $ 255 * fromIntegral j / fromIntegral (width dimension)
           ig = floor $ 255 * fromIntegral i / fromIntegral (height dimension)
           ib = floor $ 255 * 0.2

main :: IO ()
main = 
    writeAsPMM generateScene
