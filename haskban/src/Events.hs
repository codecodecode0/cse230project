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

sampleBoard :: Board
sampleBoard = Board
  { todo = [TaskInitData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskInitData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , inProgress = [TaskInitData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , done = [TaskInitData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  }
-- Define the initial state
sampleState :: AppState
sampleState = TaskBoard sampleBoard

handleApp :: BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleApp = \case
    AppEvent _ -> return ()
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
        modify(\s -> AddTaskForm (mkForm $ TaskInitData (pack "New Task") (pack "New desc") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low))
    VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) ->
        modify(\s -> case s of
                AddTaskForm form -> TaskBoard (sampleBoard { todo = (formState form) : todo sampleBoard })
                _ -> TaskBoard sampleBoard 
            )

    VtyEvent (Vty.EvKey (Vty.KChar 'w') [Vty.MCtrl]) ->
        modify(\s -> TaskBoard sampleBoard 
            )
        
    _ -> return ()
        -- do
        -- maybeForm <- gets _appState
        -- case maybeForm of
        --     AddTaskForm form -> if allFieldsValid form
        --                  then do
        --                      let task = formState form
        --                      modify $ tasks %~ (task :)
        --                      modify $ appState .~ Nothing
        --                  else modify $ appState ?~ form
        --     Nothing -> return ()

    -- VtyEvent (Vty.EvKey (Vty.KChar 'm') [Vty.MCtrl]) -> 
    --     modify(\s -> )
    -- ev -> modify(\s -> do
    --     newState <- case s of
    --         AddTaskForm form -> do
    --             -- Handle form events and update the form state
    --             newForm <- handleFormEvent ev form
    --             return $ case formState newForm of
    --                 Just updatedData -> AddTaskForm (newForm { formState = Just updatedData })
    --                 Nothing -> s
    --         _ -> return s
    --     modify (\s' -> case newState of
    --         AddTaskForm _ -> newState
    --         _ -> s'
    --         ))

    -- ev -> do
    --     handleFormEvent ev
        -- modify(\s -> AddTaskForm (handleFormEvent ev))
        -- st <- gets formState

    -- VtyEvent e -> do
    --     newState <- case getFormState of
    --         AddTaskForm form -> handleFormEvent e form
    --         _ -> return s
    --     modify (\s' -> case s of
    --         AddTaskForm _ -> AddTaskForm newState
    --         _ -> s'
    --         )

        -- modify(\s -> case s of
        --         AddTaskForm form -> AddTaskForm (handleFormEvent e form)
        --         _ -> return s)
        
        -- modify(\s -> do 
        --     case s of
        --         AddTaskForm form -> handleFormEvent e form
        --         _ -> return s)

    -- VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) -> do
    --     case getFormState of
    --         Just form -> do
    --             let taskData = formState form
    --             -- Append the new task to the "Todo" list in the task board
    --             modify (\s -> case s of
    --                 TaskBoard board -> TaskBoard (board { todo = taskData : todo board })
    --                 _ -> s
    --                 )
    --         Nothing -> return ()
        -- modify(\s -> s)
    
    -- -- Press Fn 1 to open form
    -- VtyEvent (Vty.EvKey (Vty.KFun 1) []) ->
    --     AddTaskForm (mkForm $ TaskInitData (pack "") (pack "") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low)
        -- modify(\s -> s { AddTaskForm mkForm TaskInitData {(pack "") (pack "") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low}})
--     TaskBoard board -> handleBoard board
--     AddTaskForm form -> handleForm form

-- handleBoard :: Board -> BrickEvent ResourceName e -> EventM ResourceName () ()
-- handleBoard board ev = case ev of
--     VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
--         modify(\s -> s)
--         -- modify(\s -> s { AddTaskForm mkForm TaskInitData {(pack "") (pack "") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low}})
        
--     VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
--     _ -> return ()

-- handleForm :: TaskForm TaskInitData -> BrickEvent ResourceName e -> EventM ResourceName () ()
-- handleForm form ev = case ev of
--     VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
    -- _ -> return ()
    -- _ -> do
    --     nextForm <- handleFormEvent ev form
    --     return (AddTaskForm nextForm)