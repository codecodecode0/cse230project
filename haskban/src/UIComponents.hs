-- {-# LANGUAGE TemplateHaskell #-}

module UIComponents where
--     ( Task(..)
--     , Board(..)
--     , TaskBoard(..)
--     , TaskContent(..)
--     -- , FormState(..)
--     , drawTask
--     , drawColumn
--     , drawBoard
--     -- , drawForm
--     ) where

-- import Brick
-- import Brick.Widgets.Border
-- import Brick.Widgets.Core
-- import           Data.Text (Text, pack)  -- Import Data.Text and pack function
-- -- import Lens.Micro
-- import Control.Lens hiding (element)
-- import Control.Lens.TH
-- import Form
-- -- import           Control.Lens

-- -- Define Task and Board types
-- -- data Task = Task
-- --     { taskName :: String
-- --     , taskDescription :: String
-- --     }
-- data TaskContent = TaskContent
--     { taskName :: String
--     , taskDescription :: String
--     } deriving (Show, Eq)

-- data Board = Board
--     { todo :: [TaskContent]
--     , inProgress :: [TaskContent]
--     , done :: [TaskContent]
--     }

-- -- Define TaskBoard type
-- data TaskBoard = TaskBoard
--     { board :: Board
--     , formState :: Maybe Task
--     }


-- -- data Task = Task
-- --     { _title :: T.text
-- --     , _description :: T.text
-- --     }

-- -- makeLenses ''Task
-- -- Rendering functions
-- drawTask :: TaskContent -> Widget FormName
-- drawTask task =
--     vBox [ strWrap (taskName task)
--          , strWrap (taskDescription task)
--          , hBorder
--          ]

-- drawColumn :: [TaskContent] -> Widget FormName
-- drawColumn tasks =
--     vBox $ map drawTask tasks

-- drawBoard :: Board -> Widget FormName
-- drawBoard board =
--     hBox [ borderWithLabel (str "Todo") (drawColumn (todo board))
--          , borderWithLabel (str "In Progress") (drawColumn (inProgress board))
--          , borderWithLabel (str "Done") (drawColumn (done board))
--          ]

-- -- taskForm = newForm 
-- --       [ editTextField form^.name "Name" 
-- --       , editTextField taskDescription taskDescription Lens.string (Just 3)
-- --       ]


-- -- drawForm :: Maybe (Form Task e FormName) -> [Widget FormName]
-- -- drawForm (Just form) = formDraw form
-- -- drawForm Nothing = emptyWidget