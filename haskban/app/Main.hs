{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick
import Brick.Forms
import Brick.Focus
import Graphics.Vty as Vty
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import UIComponents
import Form

data TaskBoard = TaskBoard {
    _tasks :: [Task],
    _appState :: Maybe (Form Task () FormName)  -- Using () for the type variable 'e'
}

makeLenses ''TaskBoard

initialState :: TaskBoard
initialState = TaskBoard [Task "title1" "desc1"] Nothing

handleEvent :: BrickEvent n e -> EventM n TaskBoard ()
handleEvent event = case event of
    VtyEvent (EvKey (KChar 'q') [MCtrl]) -> halt

    VtyEvent (EvKey (KChar 'n') [MCtrl]) -> do
        let newTaskForm = mkForm $ Task "newtask" "newdesc"
        modify $ appState ?~ newTaskForm

    VtyEvent (EvKey (KChar 's') [MCtrl]) -> do
        maybeForm <- gets _appState
        case maybeForm of
            Just form -> if allFieldsValid form
                         then do
                             let task = formState form
                             modify $ tasks %~ (task :)
                             modify $ appState .~ Nothing
                         else modify $ appState ?~ form
            Nothing -> return ()

    VtyEvent ev -> do
        -- Correctly handle the form event
        foc <- gets formFocus
        maybeForm <- gets _appState
        case maybeForm of
            Just f -> do
                newForm <- handleFormEvent ev
                modify $ \s -> s {_appState = Just newForm}
            Nothing -> return ()

    _ -> return ()

drawUI :: TaskBoard -> [Widget FormName]
drawUI tb = case tb^.appState of
    Just form -> [renderForm form]
    Nothing   -> [drawBoard $ tb^.tasks]

app :: App TaskBoard (Form Task e FormName) e FormName
app = App
    { appDraw = drawUI
    , appHandleEvent = handleEvent
    , appChooseCursor = focusRingCursor formFocus
    -- , appChooseCursor = showFirstCursor
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

main :: IO ()
main = defaultMain app initialState >> return ()
