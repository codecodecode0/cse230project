{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Defs where

import qualified Data.Text as T
import Data.Time
import Lens.Micro ((^.))
import Lens.Micro.TH
import qualified Graphics.Vty as V
import Brick.Forms


data ResourceName 
  = FormTitle
  | FormDescription
  | FormStatus
  | FormDueDate
  | FormAssignedToId
  | FormPriority
  deriving (Eq, Ord, Show)
    
type TaskForm a e = Form a e ResourceName

data TaskStatus = Todo
                | InProgress
                | Completed
                deriving (Eq, Show)

data TaskPriority = Low
                  | Medium
                  | High
                  deriving (Eq, Show)

data TaskInitData = TaskInitData {
    _title :: T.Text,
    _description :: T.Text,
    _status :: TaskStatus,
    _dueDate :: UTCTime,
    _assignedToId :: T.Text,
    _priority :: TaskPriority
}
makeLenses ''TaskInitData

data Board = Board
  { todo :: [TaskInitData]
  , inProgress :: [TaskInitData]
  , done :: [TaskInitData]
  }

data AppState 
  = TaskBoard Board
  | AddTaskForm (TaskForm TaskInitData ())