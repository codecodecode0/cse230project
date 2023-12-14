{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw where

import Defs
import Brick
import Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Forms
import Lens.Micro ((^.))
import Data.Text (Text)
-- import qualified Data.Text as T


drawApp :: AppState -> [Widget ResourceName]
drawApp g = case g of
    TaskBoard board -> drawBoard board
    AddTaskForm form -> drawForm form

drawBoard :: Board -> [Widget ResourceName]
drawBoard board = [hBox [drawColumn "To Do" (todo board), 
                   drawColumn "In Progress" (inProgress board), 
                   drawColumn "Done" (done board)]]
            
drawColumn :: String -> [TaskInitData] -> Widget ResourceName
drawColumn colTitle tasks =
    borderWithLabel (str colTitle) (vBox $ map drawTask tasks)

drawTask :: TaskInitData -> Widget ResourceName
drawTask task =
    vBox [ txtWrap (task ^. title)
         , txtWrap (task ^. description)
         , hBorder
         ]

drawForm :: TaskForm TaskInitData -> [Widget ResourceName]
drawForm f =  [C.vCenter $ C.hCenter form]
    where
        form = addBorder "" $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        -- help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        -- body = str $ "- Title is free-form text\n" <>
        --              "- Description is free-form text\n" <>
        --              "  Status is a radio field with 3 options\n" <>
        --              "- Due Date is a time type field\n" <>
        --              "- AssignedTo is a free-form text\n" <>
        --              "- Priority is a radio field with 3 options\n" <>
        --              "- Enter/Esc quit, mouse interacts with fields"
    
addBorder :: Text -> Widget ResourceName -> Widget ResourceName
addBorder t = withBorderStyle BS.unicodeRounded . borderWithLabel (txt t)  