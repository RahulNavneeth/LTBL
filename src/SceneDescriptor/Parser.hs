module SceneDescriptor.Parser (parser) where 

import System.FilePath (takeExtension)

fileExtension :: String
fileExtension = ".ltbl"

parser :: IO ()
parser = do
	let inputFilePath = "./scene/frame1.json"
	-- inputFilePath <- getLine
	if takeExtension inputFilePath /= fileExtension
		then putStrLn $ "Err: Program only accepts " ++ fileExtension ++ " files"
		else do
			rawLtblData <- readFile inputFilePath
			print rawLtblData
	
