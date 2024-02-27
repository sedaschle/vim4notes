module Spellcheck where
    
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, fromList)
import System.Directory (doesFileExist)
import Data.List (break)

loadfrequencies :: FilePath -> IO (Map String Double)
loadfrequencies filepath = do
    dictString <- readFile filepath
    let dict = read dictString :: (Map String Double)
    return dict









---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
-----------------------------   PREPROCESSING CSV FILE, THIS WILL NOT RUN AGAIN       -------------------------------------
---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
splitToTuple :: String -> (String, Double)
splitToTuple str =
    let tot = 588124220187 in
        let (beforeComma, afterComma) = break (== ',') str
        in (beforeComma, (read (tail afterComma) :: Double) / tot)


readListFromFile :: FilePath -> IO (Map String Double)
readListFromFile filename = do
    fileExists <- doesFileExist filename
    if fileExists then do
        contents <- readFile filename
        let contentLines = lines contents
        let pairs = Prelude.map splitToTuple contentLines
        let dict = fromList pairs
        return dict
    else do
        return (fromList [])

preprocess :: FilePath -> IO [a]
preprocess filepath = do
    dict <- readListFromFile filepath
    let dictstring = show dict 
    writeFile "freq_dict.txt" dictstring
    return []


