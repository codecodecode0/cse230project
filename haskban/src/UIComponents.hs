{-# LANGUAGE TemplateHaskell #-}

module UIComponents
    ( Task(..)
    , Board(..)
    , TaskBoard(..)
    , FormState(..)
    , drawTask
    , drawColumn
    , drawBoard
    , drawForm
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import           Data.Text (Text, pack)  -- Import Data.Text and pack function
-- import Lens.Micro
import Control.Lens hiding (element)
import Control.Lens.TH
import Form
-- import           Control.Lens

-- Define Task and Board types
-- data Task = Task
--     { taskName :: String
--     , taskDescription :: String
--     }

data Board = Board
    { todo :: [Task]
    , inProgress :: [Task]
    , done :: [Task]
    }

-- Define TaskBoard type
data TaskBoard = TaskBoard
    { board :: Board
    , formState :: Maybe Task
    }


-- data Task = Task
--     { _title :: T.text
--     , _description :: T.text
--     }

-- makeLenses ''Task
-- Rendering functions
drawTask :: Task -> Widget n
drawTask task =
    vBox [ strWrap (taskName task)
         , strWrap (taskDescription task)
         , hBorder
         ]

drawColumn :: [Task] -> Widget n
drawColumn tasks =
    vBox $ map drawTask tasks

drawBoard :: Board -> Widget n
drawBoard board =
    hBox [ borderWithLabel (str "Todo") (drawColumn (todo board))
         , borderWithLabel (str "In Progress") (drawColumn (inProgress board))
         , borderWithLabel (str "Done") (drawColumn (done board))
         ]

-- taskForm = newForm 
--       [ editTextField form^.name "Name" 
--       , editTextField taskDescription taskDescription Lens.string (Just 3)
--       ]


-- drawForm :: Maybe (Form Task e FormName) -> [Widget FormName]
-- drawForm (Just form) = formDraw form
-- drawForm Nothing = emptyWidget