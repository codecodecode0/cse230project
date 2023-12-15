{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Defs where

import qualified Data.Text as T
import Data.Time
import Lens.Micro ((^.))
import Lens.Micro.TH
import qualified Graphics.Vty as V
import Brick.Forms
import Data.Aeson
import GHC.Generics

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
                deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TaskPriority = Low
                  | Medium
                  | High
                  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TaskData = TaskData {
  _title :: T.Text,
  _description :: T.Text,
  _status :: TaskStatus,
  _dueDate :: Maybe UTCTime,
  _assignedToId :: T.Text,
  _priority :: TaskPriority
}
makeLenses ''TaskData

data ReceiveBody = Receive {
    _taskTitle :: String,
    _taskDescription :: Maybe String,
    _taskOwnerId :: String,
    _taskStatus :: TaskStatus,
    _taskDueDate :: Maybe UTCTime,
    _taskAssignedToId :: Maybe String, 
    _taskPriority :: TaskPriority
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeLenses ''ReceiveBody

data TaskBody = Task {
    _taskId :: Maybe Int,
    _receievedBody :: ReceiveBody,
    _taskCreatedAt :: UTCTime,
    _taskUpdatedAt :: UTCTime
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeLenses ''TaskBody

data Board = MkBoard
  { _todo :: [TaskData]
  , _inProgress :: [TaskData]
  , _done :: [TaskData]
  , _pointer :: [Int]
  }
makeLenses ''Board

data FormEvent = Int

data CurrentState
  = BoardState | AddFormState | FilterState | EditFormState | HelpState

data FilterFormData = FilterFormData {
  _filterAssignedToIdForm :: T.Text
}
makeLenses ''FilterFormData


data AppState = MkAppState {
  _board :: Board,
  _form :: TaskForm TaskData FormEvent,
  _state :: CurrentState,
  _fullBoardCopy :: Board,
  _filteredBoard :: Board,
  _filterForm :: TaskForm FilterFormData FormEvent
}
makeLenses ''AppState

