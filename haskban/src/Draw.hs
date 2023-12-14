{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw where

import Defs
import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Forms
import Brick.Focus
import Lens.Micro ((^.))
import Data.Text (Text)


drawApp :: AppState -> [Widget ResourceName]
drawApp g = case g of
    TaskBoard board -> drawBoard board
    AddTaskForm form -> drawForm form

drawBoard :: Board -> [Widget ResourceName]
drawBoard board = [C.vCenter $ C.hCenter bd <=> C.hCenter help]
    where
        bd = hBox [drawColumn "To Do" (todo board), 
                   drawColumn "In Progress" (inProgress board), 
                   drawColumn "Done" (done board)]
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "Press Ctrl + N to create a new task\n" <>
                     "Press Ctrl + I to open the In Progress tasks\n" <>
                     "Press Ctrl + T to open the To Do tasks\n" <>
                     "Press Ctrl + Q to quit the app"
            
drawColumn :: String -> [TaskData] -> Widget ResourceName
drawColumn colTitle tasks =
    borderWithLabel (str colTitle) (vBox $ map drawTask tasks)

drawTask :: TaskData -> Widget ResourceName
drawTask task =
    vBox [ txtWrap (task ^. title)
         , txtWrap (task ^. description)
         , hBorder
         ]

drawForm :: TaskForm TaskFormData e -> [Widget ResourceName]
drawForm f =  [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = addBorder "" $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "Press Enter or Tab to move to the next field\n" <>
                     "Press Ctrl + S to save the task\n" <>
                     "Press Ctrl + B to discard changes and return to tasks board\n" <>
                     "Press Ctrl + Q to quit the app"
    
addBorder :: Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BS.unicodeRounded . borderWithLabel (txt t)