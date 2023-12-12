{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Server.Server where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)
import           Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, readTVar, writeTVar, atomically)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Aeson (encode, decode)
import           Schema
import           Data.Time (UTCTime)
import qualified Text.Read as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL


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

initTasks :: Task
initTasks = M.fromList [(0, task1), (1, task2)]

allProfiles :: Profile
allProfiles = M.fromList [(0, "Viraj"), (1, "Someone")]

readIntOnly :: B.ByteString -> Maybe Int
readIntOnly bs = case T.readMaybe (B.unpack bs) :: Maybe Int of
    Nothing -> Nothing
    Just i -> Just i

parseQueryItem :: QueryItem -> Maybe Int
parseQueryItem (k, v) = case k of
    "id" -> case v of
        Nothing -> Nothing
        Just bs -> readIntOnly bs
    _ -> Nothing

getTask :: TVar Task -> Maybe Int -> [TaskBody]
getTask _ Nothing = []
getTask allTasks (Just tid) = do 
    let tasks = unsafePerformIO (readTVarIO allTasks) 
    case (tid, M.lookup tid tasks) of
        (_, Just task_body) -> [task_body]
        (-1, Nothing) -> M.elems tasks
        _ -> []

handleGETrequest :: TVar Task -> Request -> Response
handleGETrequest allTasks request = do
    let queries = queryString request
    let tid = parseQueryItem (L.head queries)
    let queriedTask = getTask allTasks tid
    if null queriedTask then
        responseLBS status404 [("Content-Type", "text/plain")] "Task not found" 
    else
        responseLBS status200 [("Content-Type", "application/json")] (encode queriedTask)

createTask :: TVar Task -> TaskBody -> IO ()
createTask allTasks newTask = do
    atomically writeTask
    where
        writeTask = do
            tasks <- readTVar allTasks
            let allkeys = M.keys tasks
            let newId = if null allkeys then 0 else maximum allkeys + 1
            let updatedTasks = M.insert newId newTask tasks
            (writeTVar allTasks updatedTasks)

app :: TVar Task -> Application
app allTasks request respond = case requestMethod request of
    "GET" -> respond (handleGETrequest allTasks request)
    "POST" -> do
        body <- strictRequestBody request
        case decode body of
            Just newTask -> (createTask allTasks newTask) >> (putStrLn $ "Received new task: " ++ show (newTask :: TaskBody)) >> respond (responseLBS status200 [("Content-Type", "text/plain")] "Task created successfully")
            Nothing -> putStrLn "Invalid request body" >> respond (responseLBS status400 [("Content-Type", "text/plain")] "Invalid request body")
    _ -> do
        respond (responseLBS status405 [("Content-Type", "test/plain")] "Method Not Allowed")


-- TODO: what to do if 8080 is already in use?
serverMain :: IO ()
serverMain = do
    allTasks <- newTVarIO initTasks -- Initialize an empty task board
    putStrLn "Starting server on port 8080 (http://localhost:8080). Press Ctrl-C to quit."
    run 8080 (app allTasks)
