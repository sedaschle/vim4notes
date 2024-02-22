module Notebook where

-- To run it, try:
-- ghci
-- :load Notebook
-- go

import System.IO
import System.IO.Error
import System.Directory
import System.Environment

data NoteTree = Empty | NoteNode String [NoteTree] deriving (Show, Read)

readListFromFile :: FilePath -> IO String
readListFromFile filename = do
    fileExists <- doesFileExist filename
    case fileExists of
        True -> readFile filename
        False -> return ""

go :: IO NoteTree
go = do
    let filename = "initNotes.txt"
    noteString <- readListFromFile filename
    let newTree = read noteString :: NoteTree
    putStrLn (show newTree)
    return newTree