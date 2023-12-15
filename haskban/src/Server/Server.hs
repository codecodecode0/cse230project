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
import           Data.Time (getCurrentTime)
import qualified Text.Read as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B


task1 :: TaskBody
task1 = Task {
    _taskId = Just 0,
    _taskCreatedAt = read "2023-12-12 00:00:00",
    _taskUpdatedAt = read "2023-12-12 00:00:00",
    _receievedBody = Receive {
        _taskTitle = "Task 1",
        _taskDescription = Just "Some Description",
        _taskOwnerId = "Viraj",
        _taskStatus = Todo,
        _taskDueDate = Nothing,
        _taskAssignedToId = Nothing,
        _taskPriority = Low
    }
}

task2 :: TaskBody
task2 = Task {
    _taskId = Just 1,
    _taskCreatedAt = read "2023-12-12 00:00:00",
    _taskUpdatedAt = read "2023-12-12 00:00:00",
    _receievedBody = Receive {
        _taskTitle = "Task 2",
        _taskDescription = Just "Some Description",
        _taskOwnerId = "Viraj",
        _taskStatus = Todo,
        _taskDueDate = Nothing,
        _taskAssignedToId = Nothing,
        _taskPriority = High
    }
}

task3 :: TaskBody
task3 = Task {
    _taskId = Just 2,
    _taskCreatedAt = read "2023-12-12 00:00:00",
    _taskUpdatedAt = read "2023-12-12 00:00:00",
    _receievedBody = Receive {
        _taskTitle = "Task 2",
        _taskDescription = Just "This is an in progress task.",
        _taskOwnerId = "Viraj",
        _taskStatus = InProgress,
        _taskDueDate = Nothing,
        _taskAssignedToId = Nothing,
        _taskPriority = Low
    }
}

task4 :: TaskBody
task4 = Task {
    _taskId = Just 3,
    _taskCreatedAt = read "2023-12-12 00:00:00",
    _taskUpdatedAt = read "2023-12-12 00:00:00",
    _receievedBody = Receive {
        _taskTitle = "Completed Task",
        _taskDescription = Just "This task is completed. Yay!",
        _taskOwnerId = "Viraj",
        _taskStatus = Completed,
        _taskDueDate = Nothing,
        _taskAssignedToId = Nothing,
        _taskPriority = Low
    }
}



initTasks :: Task
initTasks = M.fromList [(0, task1), (1, task2), (2, task3), (3, task4)]

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

createTask :: TVar Task -> ReceiveBody -> IO ()
createTask allTasks newTask = do
    atomically writeTask
    where
        writeTask = do
            tasks <- readTVar allTasks
            let allkeys = M.keys tasks
            let newId = if null allkeys then 0 else maximum allkeys + 1
            let toAddTask = Task {
                _taskId = Just newId,
                _receievedBody = newTask,
                _taskCreatedAt = current_time,
                _taskUpdatedAt = current_time
            }
            let updatedTasks = M.insert newId toAddTask tasks
            (writeTVar allTasks updatedTasks)
        current_time = unsafePerformIO getCurrentTime


-- updateTask :: TVar Task -> TaskBody -> IO ()
-- updateTask allTasks currTask = do
--     atomically writeTask
--     where
--         writeTask = do
--             tasks <- readTVar allTasks
--             let currId = taskId currTask
--             case (currId, M.lookup tasks currId) of
--                 (Nothing, _) -> return ()
--                 (_, Nothing) -> return ()
--                 (Just tid, Just prevTask) -> do
--                     let toAddTask = Task {
--                         taskId = tid,
--                         receievedBody = currTask,
--                         taskCreatedAt = prevTask.taskCreatedAt,
--                         taskUpdatedAt = getCurrentTime
--                     }
--                     let updatedTasks = M.insert tid toAddTask tasks
--                     (writeTVar allTasks updatedTasks)

-- handleDELETErequest :: TVar Task -> Request -> Response
-- handleDELETErequest allTasks request = do
    


deleteTask :: TVar Task -> Maybe Int -> IO ()
deleteTask _ Nothing = putStrLn "Invalid task id"
deleteTask allTasks (Just tid) = do
    atomically writeTask
    where
        writeTask = do
            tasks <- readTVar allTasks
            case M.lookup tid tasks of
                Nothing -> return ()
                Just _ -> do
                    let updatedTasks = M.delete tid tasks
                    (writeTVar allTasks updatedTasks)

app :: TVar Task -> Application
app allTasks request respond = case requestMethod request of
    "GET" -> respond (handleGETrequest allTasks request)
    "POST" -> do
        body <- strictRequestBody request
        case decode body of
            Just newTask -> (createTask allTasks newTask) >> respond (responseLBS status200 [("Content-Type", "text/plain")] "Task created successfully")
            Nothing -> putStrLn "Invalid request body" >> respond (responseLBS status400 [("Content-Type", "text/plain")] "Invalid request body")
    -- "PUT" -> do
    --     body <- strictRequestBody request
    --     case decode body of
    --         Just currTask -> (updateTask allTasks currTask) >> respond (responseLBS status200 [("Content-Type", "text/plain")] "Task updated successfully")
    --         Nothing -> putStrLn "Invalid request body" >> respond (responseLBS status400 [("Content-Type", "text/plain")] "Invalid request body")
    "DELETE" -> do
        let queries = queryString request
        let tid = parseQueryItem (L.head queries)
        deleteTask allTasks tid
        respond (responseLBS status200 [("Content-Type", "text/plain")] "Task deleted successfully")
    _ -> do
        respond (responseLBS status405 [("Content-Type", "test/plain")] "Method Not Allowed")


-- TODO: what to do if 8080 is already in use?
serverMain :: IO ()
serverMain = do
    allTasks <- newTVarIO initTasks -- Initialize an empty task board
    putStrLn "Starting server on port 8080 (http://localhost:8080). Press Ctrl-C to quit."
    run 8080 (app allTasks)

{-
GET http://localhost:8080/?id=0 -> get task with id 0
GET http://localhost:8080/?id=-1 -> get all tasks

POST http://localhost:8080/ -> create a new task
DELETE http://localhost:8080/?id=0 -> delete task with id 0


TODO
PUT http://localhost:8080/?id=0 -> update task with id 0


-}