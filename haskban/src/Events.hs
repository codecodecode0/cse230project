{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Events where

import Defs
import Brick
import Brick.Forms
import Lens.Micro
import qualified Graphics.Vty as Vty
import Control.Monad (void)
import Data.Text (pack)
import Form
import Brick
import Brick (modify)

handleApp :: BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleApp = \case
    AppEvent _ -> return ()
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    ev -> do
        as <- get
        case (as ^. state) of
            BoardState -> handleBoard as ev
            FormState  -> handleForm as ev

handleBoard :: AppState -> BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleBoard as ev = do
    let curr_board = (as ^. board)
    let currentPointer = curr_board ^. pointer
    let currentPointerX = (currentPointer !! 0)
    let currentPointerY = (currentPointer !! 1)
    let todos = curr_board ^. todo
    let progs = curr_board ^. inProgress
    let dones = curr_board ^. done

    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
            put (as & state .~ FormState & form .~ (mkForm $ TaskFormData (pack "") (pack "") Low Todo (pack "")))
            -- modify (\s -> (mkForm $ TaskFormData (pack "") (pack "") Low 0 board Todo (pack "")))

        VtyEvent (Vty.EvKey Vty.KUp []) -> do
            let updatedPointer = [currentPointerX, max 0 (currentPointerY - 1)]
            put (as & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KDown []) -> do
            let maxPossLen = getMaxPossibleLen curr_board currentPointerX
            let updatedPointer = [currentPointerX, min maxPossLen (currentPointerY + 1)]
            put (as & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KRight []) -> do
            let updatedPointerX = min 2 (currentPointerX + 1)
            let updatedPointerY = min (getMaxPossibleLen curr_board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            put (as & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KLeft []) -> do
            let updatedPointerX = max 0 (currentPointerX - 1)
            let updatedPointerY = min (getMaxPossibleLen curr_board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            put (as & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) -> do
            let movedCols = moveToRight todos progs dones currentPointerX currentPointerY
            -- put (as & board . pointer .~ updatedPointer)
            put (as & board .~ (curr_board { _todo = movedCols !! 0, _inProgress = movedCols !! 1, _done = movedCols !! 2, _pointer = [0, 0]}))
            -- modify(\s -> TaskBoard (curr_board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

        VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) -> do
            let movedCols = moveToLeft todos progs dones currentPointerX currentPointerY
            put (as & board .~ (curr_board { _todo = movedCols !! 0, _inProgress = movedCols !! 1, _done = movedCols !! 2, _pointer = [0, 0]}))
            -- modify(\s -> TaskBoard (curr_board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

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
getMaxPossibleLen curr_board x  
    | x == 0 = length (curr_board ^. todo) - 1
    | x == 1 = length (curr_board ^. inProgress) - 1
    | x == 2 = length (curr_board ^. done) - 1
    | otherwise = 0   -- should never happen

handleForm :: AppState -> BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleForm as ev = do
    let curr_form = as ^. form
    let currentForm = formState curr_form
    let currTitle = _nameForm currentForm
    let currDesc = _descForm currentForm
    let currPriority = _taskPriorityForm currentForm
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) ->
            put (as & state .~ BoardState)

        VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) -> do
            let newTask = TaskData currTitle currDesc Todo (read "2019-01-01 00:00:00 UTC") (pack "") currPriority
            put (as & board . todo %~ (++ [newTask]) & state .~ BoardState)


        _ -> zoom form $ handleFormEvent ev
