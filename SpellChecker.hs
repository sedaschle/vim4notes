module SpellChecker where

import Spellcheck 
import Note
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, fromList)
import System.Directory (doesFileExist)
import Data.List (break, sort)
import Data.Maybe

datasetpath :: String
datasetpath = "prob_map.txt"

runspellcheck :: [String] -> Int -> IO [(String, [String])]
runspellcheck lst n = do
    probs <- loaddata datasetpath
    let corrections = [(wrd, (topncorrections wrd n probs)) | wrd <- lst]
    return corrections


loaddata :: FilePath -> IO (Map String Double)
loaddata filepath = do
    dictString <- readFile filepath
    let dict = read dictString :: (Map String Double)
    return dict

-- TODO:: THIS
spellcheck note = do 
    probabilities <- loaddata datasetpath

    -- set 
    --let leastlikelylength (note content)
    let total = Map.foldr (+) 0 probabilities
    let avgprob = total / fromIntegral (Map.size probabilities)
    return avgprob