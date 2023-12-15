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
sendGETRequest :: Int -> IO [TaskBody]
sendGETRequest data_id = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest ("GET http://localhost:8080?id="++show data_id)
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
  initrequest <- parseRequest "POST http://localhost:8080"
  let request = initrequest { method = "POST"
                         , requestBody = RequestBodyLBS $ encode some_data
                         , requestHeaders = [("Content-Type", "application/json")] }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  -- putStrLn $ "The status code was: " ++
    -- show (statusCode $ responseStatus response)
  -- print $ responseBody response
  -- let received = decode $ responseBody response :: Maybe TaskBody
  -- case received of
  --   Just r -> return r
  --   Nothing -> return $ TaskBody "" "" Todo "" Nothing Nothing Low 
-- >>> sendPOSTRequest
-- The status code was: 200
-- "{\n  \"args\": {}, \n  \"data\": \"{\\\"age\\\":30,\\\"name\\\":\\\"Michael\\\"}\", \n  \"files\": {}, \n  \"form\": {}, \n  \"headers\": {\n    \"Accept-Encoding\": \"gzip\", \n    \"Content-Length\": \"27\", \n    \"Content-Type\": \"application/json\", \n    \"Host\": \"httpbin.org\", \n    \"X-Amzn-Trace-Id\": \"Root=1-657c0710-3672cd6141fccc1b665654e3\"\n  }, \n  \"json\": {\n    \"age\": 30, \n    \"name\": \"Michael\"\n  }, \n  \"origin\": \"24.43.123.86\", \n  \"url\": \"http://httpbin.org/post\"\n}\n"
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

