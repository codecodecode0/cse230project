{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmapp_state #-}
{-# HLINT ignore "Use camelCase" #-}

module Events where

import Defs
import Brick
import Brick.Forms
-- import Lens.Micro
import qualified Graphics.Vty as Vty
import Control.Monad (void, join)
import Data.Text (pack,unpack)
import Form
import Data.IORef
import ServerData
import Data.Time
import GHC.IO (unsafePerformIO)
import Graphics.Vty (Vty(refresh))
import Control.Lens hiding (zoom)

handleApp :: BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleApp = \case
    AppEvent _ -> return ()
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    ev -> do
        app_state <- get
        case (app_state ^. state) of
            BoardState -> handleBoard ev
            FormState  -> handleForm ev
            FilterState -> handleFilter ev

handleBoard :: BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleBoard ev = do
    app_state <- get
    let curr_board = (app_state^. board)
    let currentPointer = curr_board ^. pointer
    let currentPointerX = (currentPointer !! 0)
    let currentPointerY = (currentPointer !! 1)
    let todos = curr_board ^. todo
    let progs = curr_board ^. inProgress
    let dones = curr_board ^. done
    -- refreshBoard
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) ->
            put (app_state & state .~ FormState & form .~ (mkForm $ TaskFormData (pack "") (pack "") Low Todo (pack "") (Nothing)))
            -- modify (\s -> (mkForm $ TaskFormData (pack "") (pack "") Low 0 board Todo (pack "")))

        VtyEvent (Vty.EvKey (Vty.KChar 'f') [Vty.MCtrl]) ->
            put (app_state & state .~ FilterState & filterForm .~ (mkFilterForm $ FilterFormData (pack "")))
            -- modify (\s -> (mkFilterForm $ FilterFormData (pack "")))

        VtyEvent (Vty.EvKey (Vty.KChar 'y') [Vty.MCtrl]) -> 
            put (app_state & fullBoardCopy .~ curr_board) >> 
            put (app_state & board .~ (app_state ^. filteredBoard))

        VtyEvent (Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl]) ->
            put (app_state & board .~ (app_state ^. fullBoardCopy))

        VtyEvent (Vty.EvKey Vty.KUp []) -> do
            let updatedPointer = [currentPointerX, max 0 (currentPointerY - 1)]
            put (app_state & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KDown []) -> do
            let maxPossLen = getMaxPossibleLen curr_board currentPointerX
            let updatedPointer = [currentPointerX, min maxPossLen (currentPointerY + 1)]
            put (app_state & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KRight []) -> do
            let updatedPointerX = min 2 (currentPointerX + 1)
            let updatedPointerY = min (getMaxPossibleLen curr_board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            put (app_state & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        VtyEvent (Vty.EvKey Vty.KLeft []) -> do
            let updatedPointerX = max 0 (currentPointerX - 1)
            let updatedPointerY = min (getMaxPossibleLen curr_board updatedPointerX) currentPointerY
            let updatedPointer = [updatedPointerX, updatedPointerY]
            put (app_state & board . pointer .~ updatedPointer)
            -- modify(\s -> TaskBoard (curr_board { pointer = updatedPointer }))

        -- Press Fn + 5 to update the board from the server
        VtyEvent (Vty.EvKey (Vty.KFun 5) []) -> 
            refreshBoard

        VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) -> do
            let movedCols = moveToRight todos progs dones currentPointerX currentPointerY
            -- put (app_state & board . pointer .~ updatedPointer)
            put (app_state & board .~ (curr_board { _todo = movedCols !! 0, _inProgress = movedCols !! 1, _done = movedCols !! 2, _pointer = [0, 0]}))
            -- modify(\s -> TaskBoard (curr_board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

        VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) -> do
            let movedCols = moveToLeft todos progs dones currentPointerX currentPointerY
            put (app_state & board .~ (curr_board { _todo = movedCols !! 0, _inProgress = movedCols !! 1, _done = movedCols !! 2, _pointer = [0, 0]}))
            -- modify(\s -> TaskBoard (curr_board { todo = movedCols !! 0, inProgress = movedCols !! 1, done = movedCols !! 2, pointer = [0, 0]}))

        _ -> return ()

getReceiveBody :: TaskFormData -> ReceiveBody
getReceiveBody taskFormData =
    let curr_title = taskFormData ^. nameForm
        curr_desc = taskFormData ^. descForm
        curr_priority = taskFormData ^. taskPriorityForm
        curr_status = taskFormData ^. statusForm
        curr_assignedToId = taskFormData ^. assignedToIdForm
        curr_dueDate = taskFormData ^. dueDateForm
    in Receive (unpack curr_title) (Just $ unpack curr_desc) (unpack curr_assignedToId) curr_status curr_dueDate (Just $ unpack curr_assignedToId) curr_priority

refreshBoard :: EventM ResourceName AppState ()
refreshBoard = do
        app_state <- get
        let updatedBoard = filterTasks $ unsafePerformIO $ sendGETRequest
        -- let updatedBoard = filterTasks received_data
        put (app_state & board .~ updatedBoard)

taskBody_Data :: TaskBody -> TaskData
taskBody_Data taskBody =
    let curr_body = taskBody ^. receievedBody
        curr_taskTitle = curr_body^.taskTitle
        curr_taskDescription = curr_body^.taskDescription
        curr_taskStatus = curr_body^.taskStatus
        curr_taskDueDate = curr_body^.taskDueDate
        curr_taskAssignedToId = curr_body^.taskAssignedToId
        curr_taskPriority = curr_body^.taskPriority
    in TaskData (pack curr_taskTitle) (pack $ get_string curr_taskDescription) curr_taskStatus (get_time curr_taskDueDate) (pack $ get_string curr_taskAssignedToId) curr_taskPriority

get_string :: Maybe String -> String
get_string (Just s) = s
get_string Nothing = ""

get_time :: Maybe UTCTime -> UTCTime
get_time (Just t) = t
get_time Nothing = read "2099-12-31 00:00:00 UTC"

filterTasks :: [TaskBody] -> Board
filterTasks tasks =
    let todos = map taskBody_Data $ filter (\t -> (t ^. (receievedBody.taskStatus)) == Todo) tasks
        progs = map taskBody_Data $ filter (\t -> (t ^. (receievedBody.taskStatus)) == InProgress) tasks
        dones = map taskBody_Data $ filter (\t -> (t ^. (receievedBody.taskStatus)) == Completed) tasks
    in MkBoard todos progs dones [0, 0]

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

handleForm :: BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleForm ev = do
    app_state <- get
    let curr_form = app_state ^. form
    let currentForm = formState curr_form
    let currTitle = _nameForm currentForm
    let currDesc = _descForm currentForm
    let currPriority = _taskPriorityForm currentForm
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) ->
            put (app_state & state .~ BoardState)

        VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) -> do
            let newTask = TaskData currTitle currDesc Todo (read "2019-01-01 00:00:00 UTC") (pack "") currPriority
            let send_data = getReceiveBody currentForm
            let _ = sendPOSTRequest send_data
            put (app_state & board . todo %~ (++ [newTask]) & state .~ BoardState)
            

        _ -> zoom form $ handleFormEvent ev


handleFilter :: BrickEvent ResourceName FormEvent -> EventM ResourceName AppState ()
handleFilter ev = do
    app_state <- get
    let curr_form = app_state ^. filterForm
    let currentForm = formState curr_form
    let currAssignedToName = _filterAssignedToIdForm currentForm
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) ->
            put (app_state & state .~ BoardState)

        VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) -> 
            put (app_state & filteredBoard .~ (filterBoardResults (app_state ^. board) (unpack currAssignedToName)) & state .~ BoardState)


            

        _ -> zoom filterForm $ handleFormEvent ev


filterBoardResults :: Board -> String -> Board
filterBoardResults curr_board filterString =
    let todos = curr_board ^. todo
        progs = curr_board ^. inProgress
        dones = curr_board ^. done
        filteredTodos = filter (\t -> (unpack $ _assignedToId t) == filterString) todos
        filteredProgs = filter (\t -> (unpack $ _assignedToId t) == filterString) progs
        filteredDones = filter (\t -> (unpack $ _assignedToId t) == filterString) dones
    in MkBoard {
        _todo = filteredTodos,
        _inProgress = filteredProgs,
        _done = filteredDones,
        _pointer = [0, 0]
    } 