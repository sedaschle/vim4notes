module Main where

import Data.List
import Data.Char
import System.IO
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
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
  , _mode :: Mode
  , _clipboard :: Note
  , _running :: Boolean
  , _targetFile :: String
  }


runNoat :: Vim -> IO ()
runNoat vim =
    do
        if _mode vim == WRITE
            then initWriteEvent vim
            else if _mode vim == NAVIGATION
            then initNavEvent vim
            else if _mode vim == FILE
                then initFileEvent vim
                else initMenuEvent vim


modeSwitch :: Vim -> IO ()
modeSwitch vim = 
    do 
        putStrLn "Enter a mode command: "
        ans <- getLine
        if (ans `elem` ["m"])
            then do let _mode' = MENU    -- changing _mode doesn't really do anything since values in a Vim type are immutable, 
                                     -- hence we would just created a new instance of Vim with a different value of _mode where instead
                                     -- we can just call the functions directly. Keeping as is to match widget implementation for now. 
                -- initMenuEvent vim
            else if (ans `elem` ["n"])
                then do
                    let _mode' = NAVIGATION
                    -- initNavEvent vim
                    else if (ans `elem` ["f"])
                        then do 
                        let _mode' = FILE
                        -- initFileEvent vim
                        else if (ans `elem` ["w"])
                            then do
                            let _mode' = WRITE
                            -- initWriteEvent vim
                            else
                                -- let _mode' = WRITE
                                putStrLn("Invalid Command, mode set to write")
                                --initWriteEvent


initWriteEvent :: Vim -> IO ()
initWriteEvent vim = 
    do
        putStrLn "Enter a Write command: "
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

initNavEvent :: Vim -> IO()
initNavEvent vim =
    do 
        putStrLn "Enter a nav command: "
        ans <- getLine
        if (ans `elem` ["j"])
            then do
                let _currentNote' = currentNote vim
                --currentNote = getParent(currentNote);
            else if (ans `elem` ["l"])
                then do
                    let _currentNote' = currentNote vim
                    --currentNote = currentNote.getChild();
                else if (ans `elem` ["k"])
                    then do
                        let _currentNote' = currentNote vim
                        --currentNote = move(1, currentNote);
                    else if (ans `elem` ["i"])
                        then do
                            let _currentNote' = currentNote vim
                            --currentNote = move(-1, currentNote);
                        else if (ans `elem` ["/"])
                            then do 
                                putStrLn "Enter a Command: "
                                newans <- getLine
                                runCommand vim newans
        else putStrLn "Invalid Command"



initMenuEvent :: Vim -> IO ()
initMenuEvent vim =
    do
        putStrLn "Enter a menu command: "
        ans <- getLine
        if (ans `elem` ["/"])
            then do
                putStrLn "Enter a Command: "
                newans <- getLine
                runCommand vim newans
        else 
            case readMaybe ans :: Maybe Double of
                Just _ -> putStrLn "Setting current notebook"-- to be implemented  currentNotebook = library.get(Integer.parseInt(str));   currentNote = currentNotebook.getNotes().get(0);
                Nothing -> putStrLn "Enter a number corresponding to a notebook."


initFileEvent :: Vim -> IO()
initFileEvent vim  =
    do 
        let pathnames = listDirectory "./data"

        putStrLn "Enter a file command: "
        ans <- getLine  
        if (ans `elem` ["save"])
            then do
                putStrLn "Saved"
                --to be implemented
            else if (ans `elem` ["/"])
                then do
                    putStrLn "Enter a Command: "
                    newans <- getLine
                    runCommand vim newans
                else if (ans `elem` ["load"])
                    then do -- to be implemented
        else putStrLn "Invalid Command"
        
        

runCommand :: Vim -> String -> IO ()
runCommand vim inString = 
    do 
        if (inString `elem` ["print"])
            then do
                print vim
            else if (inString `elem` ["exit"])
                then do
                    return ()
                else if (inString `elem` ["delete"])
                    then do
                        delete vim
                    else if (inString `elem` ["cut"])
                        then do
                            cut vim
                        else if (inString `elem` ["paste"])
                            then do
                                past vim
                            else if (inString `elem` ["mode"])
                                then do
                                    modeSwitch vim
        else putStrLn "Not a valid command"


print :: Vim -> IO ()
print vim = 
    do 
        let note = currentNote vim
        putStrLn("Printing")
        return ()
    -- to be implemented

delete :: Vim -> IO ()
delete vim = 
    do 
        let deleteTag = noteid (_currentNote vim)  --hopefully this gets the node id of the current note in the vim instance, i got it from chat
    -- let _currentNote' = --currentNote.getParent()    --pretty sure this doesn't really work
    -- let updatedNote = --removeDent(deleteTag) 
    -- let _currentNote' = updatedNote
        return()

cut :: Vim -> IO ()
cut vim = 
    do 
        let _clipboard' = _currentNote vim
        let _clipboard vim  = _clipboard
        delete vim

paste :: Vim -> IO ()
-- paste vim = To be implemented

clear :: vim -> IO ()
-- clear vim = To be implemented



newVim :: Notebook -> Note -> Note -> Mode -> Maybe Note -> Boolean -> String -> Vim
newVim notebook currnote homenote mode clip running target = do
    let vim = Vim {
            _currentNotebook = notebook,
            _currentNote = currnote,
            _homeNote = homenote,
            _mode = mode,
            _clipboard = clip,
            _running = running,
            _targetFile = target
        }
    return vim
    


main :: IO ()
main = 
    do
        let vim = Vim{
            _currentNotebook = Notebook { title = "", notes = [] },
            _currentNote = Note {noteid = 0, content = "", section = False, children = [], parent = Nothing},
            _homeNote = Note {noteid = 0, content = "", section = False, children = [], parent = Nothing},
            _mode = WRITE,
            _clipboard = Nothing,
            _running = True,
            _targetFile = ""
        }
        runNoat vim 
