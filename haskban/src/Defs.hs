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

data Board = MkBoard
  { _todo :: [TaskData]
  , _inProgress :: [TaskData]
  , _done :: [TaskData]
  , _pointer :: [Int]
  }
makeLenses ''Board

data TaskFormData = TaskFormData {
  _nameForm :: T.Text,
  _descForm :: T.Text,
  _taskPriorityForm :: TaskPriority,
  _statusForm :: TaskStatus,
  _assignedToIdForm :: T.Text
}
makeLenses ''TaskFormData


data FormEvent = Int

data CurrentState
  = BoardState | FormState

data AppState = MkAppState {
  _board :: Board,
  _form :: TaskForm TaskFormData FormEvent,
  _state :: CurrentState
}
makeLenses ''AppState