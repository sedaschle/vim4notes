module Notebook where

-- To run it, try:
-- ghci
-- :load Notebook
-- try 

import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import Note hiding (readListFromFile)

data Notebook = Notebook {
    title :: String,
    notes :: [Note]
} deriving (Show, Read, Eq)

testNotebook :: Notebook
testNotebook = Notebook "Test" []

newNotebook :: String -> [Note] -> Notebook
newNotebook = Notebook

addNote :: Note -> Notebook -> Notebook
addNote noteRequest notebook = notebook { notes = notes notebook ++ [noteRequest] }


-- load note from text file
loadNotebook :: FilePath -> IO Notebook
loadNotebook filename =
   do
      notebookString <- readListFromFile filename
      let newNotebook = read notebookString :: Notebook
      return newNotebook

-- save notebook to text file
saveNotebook notebook filename =
   do
      let notebookString = show notebook
      writeFile filename notebookString

readListFromFile :: FilePath -> IO String
readListFromFile filename = do
    fileExists <- doesFileExist filename
    (if fileExists then (do
        contents <- readFile filename
        return (filter (/= '\n') contents)) else return "")

try :: IO Notebook
try = do
    let filename = "notebook1.txt"
    newNotebook <- loadNotebook filename
    print newNotebook

    --saveNote testNotebook filename
    return testNotebook


