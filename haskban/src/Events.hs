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
handleBoard board ev = case ev of
    VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
        modify(\s -> AddTaskForm (mkForm $ TaskFormData (pack "") (pack "") Low 0 board))

    _ -> return ()

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