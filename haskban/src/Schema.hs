{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Schema where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Map (Map)

data TaskStatus = Backlog | Todo | InProgress | Completed deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TaskPriority = Low | Medium | High deriving (Show, Eq, Generic, ToJSON, FromJSON)


data TaskBody = Task {
    taskTitle :: String,
    taskDescription :: Maybe String,
    taskOwnerId :: Int,
    taskStatus :: TaskStatus,
    taskDueDate :: Maybe UTCTime,
    taskAssignedToId :: Maybe Int, 
    taskPriority :: Maybe TaskPriority,
    taskCreatedAt :: UTCTime,
    taskUpdatedAt :: UTCTime,
    taskLastUpdatedById :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Task = Map Int TaskBody
type Profile = Map Int String