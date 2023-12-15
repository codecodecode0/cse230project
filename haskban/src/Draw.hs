{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw where

import Defs
import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Brick.Forms
import Brick.Focus
import Lens.Micro ((^.))
import Data.Text (Text)
import qualified Graphics.Vty as Vty


drawApp :: AppState -> [Widget ResourceName]
drawApp as = case as ^. state of
    BoardState -> drawBoard as
    FormState -> drawForm as
    FilterState -> drawFilter as

drawBoard :: AppState -> [Widget ResourceName]
drawBoard as = [C.vCenter $ C.hCenter bd <=> C.hCenter help]
    where
        curr_board = as ^. board
        pt = curr_board ^. pointer
        bd = hBox [drawColumn pt "To Do" (curr_board ^. todo), 
                   drawColumn pt "In Progress" (curr_board ^. inProgress), 
                   drawColumn pt "Done" (curr_board ^. done)]
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "Press Ctrl + N to create a new task\n" <>
                     "Use arrow keys to select other tasks\n" <>
                     "Press Fn 5 to refresh the board manually\n" <>
                     "Press Ctrl + R to push the task to next column\n" <>
                     "Press Ctrl + L to push the task to previous column\n" <>
                     "Press Ctrl + Q to quit the app"
            
drawColumn :: [Int] -> String -> [TaskData] -> Widget ResourceName
drawColumn pt colTitle tasks =
    if length tasks == 0
    then borderWithLabel (str colTitle) (vBox [str ("No Tasks To Display")])
    else borderWithLabel (str colTitle) (vBox $ map drawTaskWithSelection (zip [0..] tasks))
    where
        drawTaskWithSelection = drawTask pt colTitle


normalAttr, selectedAttr :: AttrName
normalAttr = attrName "normal"
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap Vty.defAttr
  [ (formAttr, Vty.defAttr),
    (selectedAttr, Vty.black `on` Vty.white),
    (E.editFocusedAttr, Vty.white `on` Vty.black),
    (focusedFormInputAttr, Vty.blue `on` Vty.white)
  ]

drawTask ::  [Int] -> String -> (Int, TaskData) -> Widget ResourceName
drawTask pt colTitle (p, task) = 
    if isSelected pt colTitle p
        then withAttr selectedAttr $ vBox [txtWrap (task ^. title), txtWrap (task ^. description), hBorder]
        else vBox [txtWrap (task ^. title), txtWrap (task ^. description), hBorder]

isSelected :: [Int] -> String -> Int -> Bool
isSelected pt colTitle idx = 
    if colTitle == "To Do" && (pt !! 0) == 0
    then (pt !! 1) == idx

    else if colTitle == "In Progress" && (pt !! 0) == 1
    then (pt !! 1) == idx

    else if colTitle == "Done" && (pt !! 0) == 2
    then (pt !! 1) == idx

    else False


drawForm :: AppState -> [Widget ResourceName]
drawForm as =  [C.vCenter $ C.hCenter rendered_form <=> C.hCenter help]
    where
        curr_form = as ^. form
        rendered_form = addBorder "" $ padTop (Pad 1) $ hLimit 50 $ renderForm curr_form
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "Press Enter or Tab to move to the next field\n" <>
                     "Press Ctrl + S to save the task\n" <>
                     "Press Ctrl + B to discard changes and return to tasks board\n" <>
                     "Press Ctrl + Q to quit the app"
    
addBorder :: Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BS.unicodeRounded . borderWithLabel (txt t)

drawFilter :: AppState -> [Widget ResourceName]
drawFilter as = [C.vCenter $ C.hCenter rendered_form <=> C.hCenter help]
    where 
        curr_form = as ^. filterForm
        rendered_form = addBorder "" $ padTop (Pad 1) $ hLimit 50 $ renderForm curr_form
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "Press Enter or Tab to move to the next field\n" <>
                     "Press Ctrl + S to save the task\n" <>
                     "Press Ctrl + B to discard changes and return to tasks board\n" <>
                     "Press Ctrl + Q to quit the app"