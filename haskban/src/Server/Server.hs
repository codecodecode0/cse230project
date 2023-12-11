{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Server.Server where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)
import           Control.Concurrent.STM (atomically, newTVarIO, readTVar, modifyTVar)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (encode)
import           Data.IORef
import           Schema
import qualified Text.Read as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Control.Exception (try)


task1 :: TaskBody
task1 = Task {
    taskTitle = "Task 1",
    taskDescription = Just "Some Description",
    taskOwnerId = 1,
    taskStatus = Todo,
    taskDueDate = Nothing,
    taskAssignedToId = Nothing,
    taskPriority = Nothing,
    taskCreatedAt = read "2023-12-12 00:00:00",
    taskUpdatedAt = read "2023-12-12 00:00:00",
    taskLastUpdatedById = 1
}

task2 :: TaskBody
task2 = Task {
    taskTitle = "Task 2",
    taskDescription = Just "Some Description",
    taskOwnerId = 1,
    taskStatus = Todo,
    taskDueDate = Nothing,
    taskAssignedToId = Nothing,
    taskPriority = Nothing,
    taskCreatedAt = read "2023-12-12 00:00:00",
    taskUpdatedAt = read "2023-12-12 00:00:00",
    taskLastUpdatedById = 1
}

allTasks :: Task
allTasks = M.fromList [(0, task1), (1, task2)]

allProfiles :: Profile
allProfiles = M.fromList [(0, "Viraj"), (1, "Someone")]

readIntOnly :: B.ByteString -> Maybe Int
readIntOnly bs = case T.readMaybe (B.unpack bs) :: Maybe Int of
    Nothing -> Nothing
    Just i -> Just i

-- TODO: error handling when v is not an int
parseQueryItem :: QueryItem -> Maybe Int
parseQueryItem (k, v) = case k of
    "id" -> case v of
        Nothing -> Nothing
        Just bs -> readIntOnly bs
    _ -> Nothing


getTask :: Maybe Int -> [TaskBody]
getTask Nothing = M.elems allTasks
getTask (Just id) = case (id, M.lookup id allTasks) of
    (id, Just task) -> [task]
    (-1, Nothing) -> M.elems allTasks
    _ -> []

handleGETrequest :: Request -> Response
handleGETrequest request = do
    let queries = queryString request
    let tid = parseQueryItem (L.head queries)
    let queriedTask = getTask tid
    if null queriedTask then
        responseLBS status404 [("Content-Type", "application/json")] (encode queriedTask)
    else
        responseLBS status200 [("Content-Type", "application/json")] (encode queriedTask)

-- handlePOSTrequest = error "Not Implemented"

app :: Application
app request respond = case requestMethod request of
    "GET" -> respond (handleGETrequest request)
    "POST" -> do
        respond (responseLBS status200 [("Content-Type", "text/plain")] "test Post")
    _ -> do
        respond (responseLBS status405 [("Content-Type", "test/plain")] "Method Not Allowed")

-- TODO: what to do if 8080 is already in use?
serverMain :: IO ()
serverMain = do
    taskBoard <- newTVarIO [] -- Initialize an empty task board
    putStrLn "Starting server on port 8080 (http://localhost:8080). Press Ctrl-C to quit."
    run 8080 app
