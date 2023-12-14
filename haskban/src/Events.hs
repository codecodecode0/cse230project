{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Events where

import Defs
import Brick
import Brick.Forms
import qualified Graphics.Vty as Vty
import Data.Text (pack)
import Form
import Brick
import Brick (modify)
import Brick.Forms (Form(formState))

handleApp :: BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleApp = \case
    AppEvent _ -> return ()
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    ev -> do
        state <- get
        case state of
            TaskBoard board -> handleBoard board ev
            AddTaskForm form -> handleForm form ev

handleBoard :: Board -> BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleBoard board ev = do
    let currentPointer = pointer board
    let currentPointerX = (currentPointer !! 0)
    let currentPointerY = (currentPointer !! 1)
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
            modify(\s -> AddTaskForm (mkForm $ TaskFormData (pack "") (pack "") Low 0 board))

        VtyEvent (Vty.EvKey Vty.KUp []) -> do
            let updatedPointer = [currentPointerX, max 0 (currentPointerY - 1)]
            modify(\s -> TaskBoard (board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KDown []) -> do
            let maxPossLen = getMaxPossibleLen board currentPointerX
            let updatedPointer = [currentPointerX, min maxPossLen (currentPointerY + 1)]
            modify(\s -> TaskBoard (board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KRight []) -> do
            let updatedPointerX = min 2 (currentPointerX + 1)
            let updatedPointerY = min (getMaxPossibleLen board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            modify(\s -> TaskBoard (board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KLeft []) -> do
            let updatedPointerX = max 0 (currentPointerX - 1)
            let updatedPointerY = min (getMaxPossibleLen board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            modify(\s -> TaskBoard (board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) -> do
            let todos = todo board
            let progs = inProgress board
            let dones = done board
            let movedCols = moveToRight todos progs dones currentPointerX currentPointerY
            modify(\s -> TaskBoard (board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

        VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) -> do
            let todos = todo board
            let progs = inProgress board
            let dones = done board
            let movedCols = moveToLeft todos progs dones currentPointerX currentPointerY
            modify(\s -> TaskBoard (board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

        _ -> return ()

moveToRight :: [TaskData] -> [TaskData] -> [TaskData] -> Int -> Int -> [[TaskData]]
moveToRight todos progs dones cpx cpy = 
    if cpx == 0
    then [removeAtIndex cpy todos, progs ++ [todos !! cpy], dones]
    else if cpx == 1
    then [todos, removeAtIndex cpy progs, dones ++ [progs !! cpy]]
    else [todos, progs, dones]

moveToLeft :: [TaskData] -> [TaskData] -> [TaskData] -> Int -> Int -> [[TaskData]]
moveToLeft todos progs dones cpx cpy = 
    if cpx == 0
    then [todos, progs, dones]
    else if cpx == 1
    then [todos ++ [progs !! cpy], removeAtIndex cpy progs, dones]
    else [todos, progs ++ [dones !! cpy], removeAtIndex cpy dones]

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex index xs
  | index < 0 = xs
  | otherwise = take index xs ++ drop (index + 1) xs


getMaxPossibleLen:: Board -> Int -> Int
getMaxPossibleLen board x  
    | x == 0 = length (todo board) - 1
    | x == 1 = length (inProgress board) - 1
    | x == 2 = length (done board) - 1
    | otherwise = 0   -- should never happen

handleForm :: TaskForm TaskFormData () -> BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleForm form ev = do
    let currentForm = formState form
    let currTitle = _name currentForm
    let currDesc = _desc currentForm
    let currPriority = _taskPriority currentForm
    let currCursor = _cursor currentForm
    let currBoard = _currentBoard currentForm
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) ->
            modify(\s -> TaskBoard currBoard)

        VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) -> do
            let newTask = TaskData currTitle currDesc Todo (read "2019-01-01 00:00:00 UTC") (pack "") currPriority
            modify(\s -> TaskBoard (currBoard { todo = newTask : todo currBoard }))

        VtyEvent (Vty.EvKey Vty.KEnter []) -> do
            let nextCursor = 1 - currCursor
            modify $ \s -> AddTaskForm (mkForm $ TaskFormData currTitle currDesc Low nextCursor currBoard)

        VtyEvent (Vty.EvKey (Vty.KChar '\t') []) -> do
            let nextCursor = 1 - currCursor
            modify $ \s -> AddTaskForm (mkForm $ TaskFormData currTitle currDesc Low nextCursor currBoard)
            
        VtyEvent (Vty.EvKey (Vty.KChar c) []) -> do
            case currCursor of
                0 -> do
                    let updatedTitle = currTitle  <> pack [c]
                    modify $ \s -> AddTaskForm (mkForm $ TaskFormData updatedTitle currDesc Low currCursor currBoard)
                1 -> do
                    let updatedDesc = currDesc <> pack [c]
                    modify $ \s -> AddTaskForm (mkForm $ TaskFormData currTitle updatedDesc Low currCursor currBoard)

        _ -> return ()
