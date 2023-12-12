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


-- Define Task and Board types
data Task = Task
    { taskName :: String
    , taskDescription :: String
    }

data Board = Board
    { todo :: [Task]
    , inProgress :: [Task]
    , done :: [Task]
    }

-- Define TaskBoard type
data TaskBoard = TaskBoard
    { board :: Board
    , formState :: Maybe FormState
    }

-- Form state for creating a new task
data FormState = FormState
    { name :: String
    , description :: String
    }

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

drawForm :: Maybe FormState -> Widget n
drawForm (Just form) =
    border $ vBox [ str "Create New Task"
                  , str "Name: " <+> txt (pack $ name form)  -- Convert String to Text using pack
                  , str "Description: " <+> txt (pack $ description form)  -- Convert String to Text using pack
                  ]
drawForm Nothing = emptyWidget
