{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Schema where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Map (Map)

data TaskStatus = Todo | InProgress | Completed deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TaskPriority = Low | Medium | High deriving (Show, Eq, Generic, ToJSON, FromJSON)


data ReceiveBody = Receive {
    _taskTitle :: String,
    _taskDescription :: Maybe String,
    _taskOwnerId :: String,
    _taskStatus :: TaskStatus,
    _taskDueDate :: Maybe UTCTime,
    _taskAssignedToId :: Maybe String, 
    _taskPriority :: TaskPriority
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TaskBody = Task {
    _taskId :: Maybe Int,
    _receievedBody :: ReceiveBody,
    _taskCreatedAt :: UTCTime,
    _taskUpdatedAt :: UTCTime
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Task = Map Int TaskBody