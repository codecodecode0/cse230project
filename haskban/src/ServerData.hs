{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module ServerData where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Lens.Micro ((^.))
import qualified Defs as H
import Data.Aeson
import GHC.Generics
import Defs
import Data.Text
import Control.Exception (catch,Exception) -- Add import for Control.Exception

-- Add import for HttpExceptionRequest
-- import Network.HTTP.Client (HttpExceptionRequest)

-- data Received_test = Received {
--   args :: Value,
--   headers :: Value,
--   origin :: String,
--   url :: String
-- } deriving (Show, Eq, Generic, FromJSON, ToJSON) -- Add FromJSON instance for Received

-- send get request to localhost:8080?id=-
sendGETRequest :: IO [TaskBody]
sendGETRequest = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "GET http://localhost:8080"
  response <- httpLbs request manager
  -- putStrLn $ "The status code was: " ++
    -- show (statusCode $ responseStatus response)
  -- print $ responseBody response
  let received = decode $ responseBody response :: Maybe [TaskBody]
  case received of
    Just r -> return r
    Nothing -> return []


sendPOSTRequest :: ReceiveBody -> IO ()
sendPOSTRequest some_data = do
  manager <- newManager defaultManagerSettings
  initrequest <- parseRequest "http://localhost:8080"
  let request = initrequest { method = "POST"
                         , requestBody = RequestBodyLBS $ encode some_data
                         , requestHeaders = [("Content-Type", "application/json")] }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  -- print $ responseBody response
  -- putStrLn $ "The status code was: " ++
    -- show (statusCode $ responseStatus response)
  -- print $ responseBody response


-- >>> sendPOSTRequest Receive { _taskTitle = "TRYYYYYYYYYYYY", _taskDescription = Just "test", _taskOwnerId = "test", _taskStatus = Todo, _taskDueDate = Nothing, _taskAssignedToId = Just "test", _taskPriority = Low }
-- The status code was: 200
--



  -- "http://localhost:8080?id=-1"
  -- let request = H.parseRequest_ "http://localhost:8080?id=-1"
  -- response <- H.httpLbs request manager
  -- putStrLn $ "The status code was: " ++
  --   show (H)
  --   -- show (H.statusCode $ H.responseStatus response)
  -- print $ H.responseBody response 

-- mkServerData :: IO ServerData
-- mkServerData = do
--   manager <- H.newManager H.defaultManagerSettings
--   return $ ServerData manager "http://localhost:3000"

