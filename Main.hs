import Data.List (intercalate)

type Pixel = Int
newtype RGB = RGB (Int, Int, Int) deriving Show
-- TODO: Flatten the scene -> [width * height] | (i:row * width + j:col)
type Scene = [[RGB]]

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


toMultilineString :: Scene -> String
toMultilineString = intercalate "\n" . map (unwords . map rgbToString)
  where
    rgbToString (RGB (a, b, c)) = unwords (map show [a, b, c])

writeAsPMM :: Scene -> IO ()
writeAsPMM scene = writeFile outputPath $
                             "P3\n"
                             ++ show (width dimension) ++ " " ++ show (height dimension) ++ "\n" ++
                             "255\n" ++
                             toMultilineString scene

generateScene :: Scene
generateScene =
  [[ value i j
    | j <- [0 .. width dimension - 1] ]
    | i <- [height dimension - 1, height dimension - 2 .. 0] ]
  where
    value i j = RGB (ir, ig, ib)
      where
        ir = floor $ 255 * fromIntegral j / fromIntegral (width dimension)
        ig = floor $ 255 * fromIntegral i / fromIntegral (height dimension)
        ib = floor $ 255 * 0.2

main :: IO ()
main = 
    writeAsPMM generateScene
