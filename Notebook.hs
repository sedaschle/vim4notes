module Notebook where

-- To run it, try:
-- ghci
-- :load Notebook
-- go

import System.IO
import System.IO.Error
import System.Directory
import System.Environment

data NoteTree = Empty | NoteLeaf String | NoteNode String [NoteTree] deriving (Show, Read, Eq)

-- just a test tree to make sure 'save' is working
testTree :: NoteTree
testTree = NoteNode "Notebook"
   [NoteNode "Subject1"
      [NoteNode "Lecture1"
         [NoteLeaf "Line1", NoteLeaf "Line2", NoteLeaf "TEST3"],
      NoteNode "Lecture2" []],
   NoteNode "Subject2" [NoteNode "Lecture1" []]]


getLineFixed =
   do
     fixdel <$> getLine

-- fixdel removes deleted elements from string
-- citation: A3 TwentyQs solutions
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r

-- load tree from text file
loadTree filename =
   do
      noteString <- readListFromFile filename
      let newTree = read noteString :: NoteTree
      return newTree

-- save tree to text file
saveTree tree filename =
   do
      let treeString = show tree
      writeFile filename treeString

readListFromFile :: FilePath -> IO String
readListFromFile filename = do
    fileExists <- doesFileExist filename
    (if fileExists then (do
        contents <- readFile filename
        return (filter (/= '\n') contents)) else return "")

go :: IO NoteTree
go = do
    let filename = "initNotes.txt"
    newTree <- loadTree filename
    print newTree

    saveTree testTree filename
    return newTree

