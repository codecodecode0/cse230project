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
    
    VtyEvent (Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl]) ->
        modify(const sampleState )

    ev -> do
        state <- get
        case state of
            TaskBoard b -> modify id
            AddTaskForm form -> do

                case ev of
                    VtyEvent (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) ->
                        modify(\s -> TaskBoard (sampleBoard { todo = formState form : todo sampleBoard }))

                    _ -> do
                        -- handleForm ev form
                        -- newState <- gets formState
                        modify $ \s -> AddTaskForm (mkForm $ TaskInitData (pack "New Task Modified") (pack "New desc mod") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low)

                -- f <- gets formFocus
                
    _ -> return ()


-- handleForm :: BrickEvent ResourceName e -> EventM ResourceName (Form TaskInitData e ResourceName) ()
-- handleForm ev = do
--                         (handleFormEvent :: (Eq n) => BrickEvent n e -> EventM n (Form TaskInitData e n) ()) ev

    -- ev -> modify (\s -> case s of
    --     TaskBoard b -> s
    --     AddTaskForm form -> 
    --         handleFormEvent ev
    --         let st = gets formState
    --         return st
            
            -- f <- gets formFocus
            -- case ev of
            --     VtyEvent (V.EvResize {}) -> return ()
            --     VtyEvent (V.EvKey V.KEsc []) -> halt
            --     -- Enter quits only when we aren't in the multi-line editor.
            --     VtyEvent (V.EvKey V.KEnter [])
            --         | focusGetCurrent f /= Just AddressField -> halt
            --     _ -> do
            --         handleFormEvent ev

            --         -- Example of external validation:
            --         -- Require age field to contain a value that is at least 18.
            --         st <- gets formState
            --         modify $ setFieldValid (st^.age >= 18) AgeField
        -- )
        
    

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