module Notebook where 

-- To run it, try:
-- ghci
-- :load Notebook
-- go

import System.IO

data NoteTree = NoteLeaf String
            | NoteNode String NoteTree NoteTree
       deriving (Show, Read)

-- TODO: make tree k-ary
initNoteTree = NoteNode "NoteBook"
                (NoteNode "Subject1"
                    (NoteNode "Lecture1"
                        (NoteLeaf "line1")
                        (NoteLeaf "line2"))
                    (NoteNode "Lecture2"
                        (NoteLeaf "Line1")
                        (NoteLeaf "Line2")))
                (NoteNode "Subject2"
                    (NoteNode "Lecture1"
                        (NoteLeaf "line1")
                        (NoteLeaf "line2"))
                    (NoteNode "Lecture2"
                        (NoteLeaf "Line1")
                        (NoteLeaf "Line2")))

run :: NoteTree -> IO NoteTree
run tree =
   do
      putStrLn "Do you want to take notes?"
      ans <- getLine
      return tree

     
go :: IO NoteTree
go = run initNoteTree
