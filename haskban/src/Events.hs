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

handleApp :: BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleApp = \case
    AppEvent _ -> return ()
    VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> 
        modify(\s -> s)
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> halt
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
    _ -> halt
    -- _ -> do
    --     nextForm <- handleFormEvent ev form
    --     return (AddTaskForm nextForm)

-- handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) = do
--     modify (\s -> s { formState = Just Task{ _title = T.empty, _description = T.empty}})
-- handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])) = halt
-- handleEvent _ = return ()