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

data TaskData = TaskData {
  _title :: T.Text,
  _description :: T.Text,
  _status :: TaskStatus,
  _dueDate :: UTCTime,
  _assignedToId :: T.Text,
  _priority :: TaskPriority
}
makeLenses ''TaskData

data Board = Board
  { todo :: [TaskData]
  , inProgress :: [TaskData]
  , done :: [TaskData]
  , pointer :: [Int]
  }

data TaskFormData = TaskFormData {
  _name :: T.Text,
  _desc :: T.Text,
  _taskPriority :: TaskPriority,
  _cursor :: Int,
  _currentBoard :: Board
}
makeLenses ''TaskFormData

data AppState 
  = TaskBoard Board
  | AddTaskForm (TaskForm TaskFormData ())
  | EditTaskForm (TaskForm TaskFormData ())