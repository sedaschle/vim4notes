module Main where

import Data.List
import Data.Char
import System.IO
import Notebook
import Note

import Text.Read (readMaybe)

data Mode = WRITE 
            | NAVIGATION 
            | MENU 
            | FILE 
            deriving (Eq, Show)

data Vim = Vim
  { _currentNotebook :: Notebook
  , _currentNote :: Note
  , _homeNote :: Note
  , _clipboard :: Maybe Note
  , _targetFile :: String
  }


runNoat :: Vim -> IO ()
runNoat vim =
    do
        initNavEvent vim


modeSwitch :: Vim -> IO ()
modeSwitch vim = do
    putStrLn "Enter a mode command [m, n, f, w]: "
    ans <- getLine
    let _mode' = case ans of
                    "m" -> MENU
                    "n" -> NAVIGATION
                    "f" -> FILE
                    "w" -> WRITE
                    _   -> WRITE  -- Default mode if the input is invalid
    -- Now you can call the appropriate function based on the mode
    case _mode' of
        MENU       -> initMenuEvent vim
        NAVIGATION -> initNavEvent vim
        FILE       -> initFileEvent vim
        WRITE      -> initWriteEvent vim


initWriteEvent :: Vim -> IO ()
initWriteEvent vim = 
    do
        putStrLn "Enter a Write command: "
        ans <- getLine
        if (ans `elem` ["/"])
            then do
                runCommand vim
                putStrLn("Running Commands: ")

        else putStrLn("Invalid Command")
        --if to be implemented  based on:
        -- if (entry.length() == 0) {
        --     if (currentNote.getParent() != null) {
        --         currentNote = currentNote.getParent();
        --         directory();
        --     }
        -- } else if (entry.charAt(0) == '\t') {
        --     clear();
        --     currentNote.addDent(entry.substring(1), true);
        --     currentNote = currentNote.getChild();
        --     directory();
        -- } else {
        --     clear();
        --     directory();
        --     currentNote.addDent(entry, false);
        -- }

initNavEvent :: Vim -> IO ()
initNavEvent vim =
    do 
        putStrLn "Enter a nav command: "
        ans <- getLine
        
        if (ans `elem` ["j"]) then do
                case getParent (_currentNote vim) of
                    Just parent -> do
                        let updatedVim = newVim (_currentNotebook vim) parent (_homeNote vim) (_clipboard vim) (_targetFile vim)
                        putStrLn "Going to Parent"
                        initNavEvent updatedVim

                    Nothing     -> do
                        putStrLn "No parent note exists."
                        initNavEvent vim
        else if (ans `elem` ["l"]) then
                case getChild (_currentNote vim) of 
                    Just child -> do
                        let updatedVim = newVim (_currentNotebook vim) child (_homeNote vim) (_clipboard vim) (_targetFile vim)
                        putStrLn "Going to Child"
                        initNavEvent updatedVim

                    Nothing     -> do 
                        putStrLn "No child note exists."
                        initNavEvent vim
        else if (ans `elem` ["k"]) then
                case getNextChild (_currentNote vim) of 
                    Just next -> do
                        let updatedVim = newVim (_currentNotebook vim) next (_homeNote vim) (_clipboard vim) (_targetFile vim)
                        putStrLn "Going to Next Node"
                        initNavEvent updatedVim

                    Nothing     -> do
                        putStrLn "No next note"
                        initNavEvent vim

        else if (ans `elem` ["i"]) then
                case getPreviousChild (_currentNote vim) of
                    Just prev -> do
                        let updatedVim = newVim (_currentNotebook vim) prev (_homeNote vim) (_clipboard vim) (_targetFile vim)
                        putStrLn "Going to Prev Node"
                        initNavEvent updatedVim

                    Nothing     -> do
                        putStrLn "No previous note"
                        initNavEvent vim

        else if (ans `elem` ["/"]) then
                runCommand vim
        else 
                putStrLn "Invalid Command" >> initNavEvent vim



initMenuEvent :: Vim -> IO ()
initMenuEvent vim =
    do
        putStrLn "Enter a menu command: "
        ans <- getLine
        if (ans `elem` ["/"])
            then do
                runCommand vim
        else 
            case readMaybe ans :: Maybe Double of
                Just _ -> putStrLn "Setting current notebook"-- to be implemented  currentNotebook = library.get(Integer.parseInt(str));   currentNote = currentNotebook.getNotes().get(0);
                Nothing -> putStrLn "Enter a number corresponding to a notebook."


initFileEvent :: Vim -> IO()
initFileEvent vim  =
    do 
        -- let pathnames = listDirectory "./data"

        putStrLn "Enter a file command: "
        ans <- getLine  
        if (ans `elem` ["save"])
            then do
                putStrLn "Saved"
                --to be implemented
            else if (ans `elem` ["/"])
                then do
                    runCommand vim
                else if (ans `elem` ["load"])
                    then do putStrLn("do")
        else putStrLn "Invalid Command"
        
        

runCommand :: Vim -> IO ()
runCommand vim = 
    do 
        putStrLn("Input Command: ")
        inString <- getLine
        if (inString `elem` ["print"])
            then do
                putStrLn "Print"
                -- print vim
            else if (inString `elem` ["exit"])
                then do
                    return ()
                else if (inString `elem` ["delete"])
                    then do
                        deleteNote vim
                    else if (inString `elem` ["cut"])
                        then do
                            cut vim
                        else if (inString `elem` ["paste"])
                            then do
                                paste vim
                            else if (inString `elem` ["mode"])
                                then do
                                    modeSwitch vim
        else putStrLn "Not a valid command"


print :: Vim -> IO ()
print vim = 
    do 
        let note = _currentNote vim
        putStrLn("Printing")
        return ()
    -- to be implemented

deleteNote :: Vim -> IO ()
deleteNote vim = 
    do 
        let deleteTag = noteid (_currentNote vim)  --hopefully this gets the node id of the current note in the vim instance, i got it from chat
    -- let _currentNote' = --currentNote.getParent()    --pretty sure this doesn't really work
    -- let updatedNote = --removeDent(deleteTag) 
    -- let _currentNote' = updatedNote
        return()

cut :: Vim -> IO ()
cut vim = putStrLn("cut")

paste :: Vim -> IO ()
paste vim = putStrLn("paste")




newVim :: Notebook -> Note -> Note -> Maybe Note -> String -> Vim
newVim notebook currnote homenote clipNote target =
    Vim {
        _currentNotebook = notebook,
        _currentNote = currnote,
        _homeNote = homenote,
        _clipboard = clipNote,
        _targetFile = target
    }
    


main :: IO ()
main = 
    do
        let vim = Vim{
            _currentNotebook = Notebook { title = "", notes = [] },
            _currentNote = Note {noteid = 0, content = "", section = False, children = [], parent = Nothing},
            _homeNote = Note {noteid = 0, content = "", section = False, children = [], parent = Nothing},
            _clipboard = Nothing,
            _targetFile = ""
        }
        runNoat vim 
