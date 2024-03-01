module Note where 
import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import Data.Time.Clock.POSIX 
import Data.Maybe (listToMaybe)
import Debug.Trace

data Note = Note {
    noteid :: Integer,
    content :: String,
    section :: Bool,
    children :: [Note],
    parent :: Maybe Note 
} deriving (Show, Read)

instance Eq Note where
    -- Two notes are equal if all of their fields are equal
    (Note id1 content1 sec1 children1 parent1) == (Note id2 content2 sec2 children2 parent2) =
        id1 == id2 &&
        content1 == content2 &&
        sec1 == sec2 &&
        children1 == children2 &&
        parent1 == parent2

-- just a test note to make sure 'save' is working
testNote :: Note
testNote = Note 1 "testing" True [Note 6 "inside" False [Note 9 "Deep in" False [] Nothing] Nothing, Note 7 "Other In" False [] Nothing, Note 8 "Other in 2" False [] Nothing ] Nothing

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


newNote :: String -> Bool -> Maybe Note -> IO Note
newNote contentRequest typeRequest parentRequest = do
    currentTime <- round <$> getPOSIXTime
    let note = Note {
        noteid = currentTime,
        content = contentRequest,
        section = typeRequest,
        children = [],
        parent = parentRequest -- TODO: MIGHT NEED FIXING if this leads to recursion pains
    }
    return note

getDirectory :: Note -> String
getDirectory note = case parent note of
    Just p -> getDirectory p ++ ">" ++ content note
    Nothing -> content note

addChild :: String -> Bool -> Note -> Note
addChild contentRequest typeRequest parentNote =
    let child = Note {
        noteid = noteid parentNote, 
        content = contentRequest,
        section = typeRequest,
        children = [],
        parent = Just parentNote
    }
    in parentNote { children = children parentNote ++ [child] }

addNote :: Note -> Note -> Note
addNote childNote parentNote =
    let childWithParent = childNote { parent = Just parentNote }
    in parentNote { children = children parentNote ++ [childWithParent] }

removeChild :: Integer -> Note -> Note
removeChild removeRequestNoteId parentNote =
    let updatedChildren = filter (\child -> noteid child /= removeRequestNoteId) (children parentNote)
    in parentNote { children = updatedChildren }

-- need to use putStrLn in debugging to make it nice
printNote :: Int -> Maybe Note -> Note -> String
printNote level current note =
    let formattedContent = if section note
                           then replicate level ' ' ++ makeUnderline (content note)
                           else replicate level ' ' ++ "- " ++ content note
        highlightedContent = if Just note == current
                             then "\x1b[31m" ++ formattedContent ++ "\x1b[0m"
                             else formattedContent
        result = highlightedContent ++ "\n" ++ concatMap (printNote (level + 1) current) (children note)
    in result
    
getParent :: Note -> Maybe Note
getParent note = parent note

getChild :: Note -> Maybe Note
getChild note = 
        getLast (children note)

getLast :: [Note] -> Maybe Note
getLast [] = Nothing
getLast xs = Just (last xs)


indexOfNote :: Note -> [Note] -> Maybe Int
indexOfNote _ [] = Nothing
indexOfNote note (x:xs)
    | note == x = Just 0
    | otherwise = fmap (+1) (indexOfNote note xs)

-- getNextChild :: Note -> Maybe Note
-- getNextChild note = case parent note of 
--     Just parentNote -> do
--         let childrenList = children parentNote
--         case indexOfNote note childrenList of
--             Just index -> listToMaybe (drop (index + 1) childrenList)
--             Nothing    -> Nothing
--     Nothing -> Nothing   

-- getPreviousChild :: Note -> Maybe Note
-- getPreviousChild note = case parent note of 
--     Just parentNote -> do
--         let childrenList = children parentNote
--         case indexOfNote note childrenList of
--             Just index -> listToMaybe (take index childrenList)
--             Nothing    -> Nothing
--     Nothing -> Nothing  

getNextChild :: Note -> Maybe Note
getNextChild note = do
    parentNote <- parent note
    let childrenList = children parentNote
    case indexOfNote note childrenList of
        Just index -> if index < length childrenList - 1
                          then Just (childrenList !! (index + 1))
                          else Nothing
        Nothing    -> Nothing

getPreviousChild :: Note -> Maybe Note
getPreviousChild note = do
    parentNote <- parent note
    let childrenList = children parentNote
    case indexOfNote note childrenList of
        Just index -> if index > 0
                          then Just (childrenList !! (index - 1))
                          else Nothing
        Nothing    -> Nothing


makeUnderline :: String -> String
makeUnderline str = "\x1B[4m" ++ str ++ "\x1B[0m"