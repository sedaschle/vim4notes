module Spellcheck where
-- CITATION 1: unigram_freq dataset from English Subtitle Word Frequency dataset (https://www.kaggle.com/datasets/lukevanhaezebrouck/subtlex-word-frequency)
-- CITATION 2: Some of the functions in the implementation are from github user doersino (https://excessivelyadequate.com/posts/spell.html)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map, fromList)
import System.Directory (doesFileExist)
import Data.List (break, sort)
import Data.Maybe


probability :: String -> Map String Double -> Double
probability word probdict =
    let prob = Map.lookup word probdict in fromMaybe 0 prob

topncorrections :: String -> Int -> (Map String Double) -> [String]
--topncorrections word n probdict = take n (reverse (sort [probability x probdict | x <- candidates word probdict]))
topncorrections word n probdict = take n $ reverse $ sort [x | x <- candidates word probdict]


-- Citation 2 - doersino
candidates :: String -> Map String Double -> [String]
candidates word probdict = concat
    [ known [word] probdict
    , known (edits1 word) probdict
    , known (edits2 word) probdict
    , [word]
    ]

-- Citation 2 - doersino       
known :: [String] -> (Map String Double) -> [String]
known words' words = [ w | w <- words', Map.member w words]

-- Citation 2 - doersino    
edits1 :: String -> [String]
edits1 word = deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, (not . null) r ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, (not . null) r, c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

-- Citation 2 - doersino 
edits2 :: String -> [String]
edits2 word = [ e2 | e1 <- edits1 word, e2 <- edits1 e1 ]


---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
-----------------------------   PREPROCESSING CSV FILE, THIS WILL NOT RUN AGAIN       -------------------------------------
---------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------
splitToTuple :: String -> (String, Double)
splitToTuple str =
    let (beforeComma, afterComma) = break (== ',') str
    in (beforeComma, (read (tail afterComma) :: Double))


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
    writeFile "prob_map.txt" dictstring
    return []


