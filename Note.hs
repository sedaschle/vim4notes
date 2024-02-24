module Note where 
import System.IO
import System.IO.Error
import System.Directory
import System.Environment

data Note = Note {
    noteid :: Integer,
    content :: String,
    section :: Bool,
    children :: [Note],
    parent :: Maybe Integer -- noteid for now
} deriving (Show, Read, Eq)

-- just a test note to make sure 'save' is working
testNote :: Note
testNote = Note 1 "testing" False [Note 6 "inside" False [] (Just 1)] Nothing

-- load note from text file
loadNote :: FilePath -> IO Note
loadNote filename =
   do
      noteString <- readListFromFile filename
      let newNote = read noteString :: Note
      return newNote

-- save tree to text file
saveNote note filename =
   do
      let noteString = show note
      writeFile filename noteString

readListFromFile :: FilePath -> IO String
readListFromFile filename = do
    fileExists <- doesFileExist filename
    (if fileExists then (do
        contents <- readFile filename
        return (filter (/= '\n') contents)) else return "")

go :: IO Note
go = do
    let filename = "note1.txt"
    newNote <- loadNote filename
    print newNote

    saveNote testNote filename
    return testNote

