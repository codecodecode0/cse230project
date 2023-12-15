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
              request <- parseRequest ("http://localhost:8080?id="++show data_id)
              response <- httpLbs request manager
              -- putStrLn $ "The status code was: " ++
                -- show (statusCode $ responseStatus response)
              -- print $ responseBody response
              let received = decode $ responseBody response :: Maybe [TaskBody]
              case received of
                Just r -> return r
                Nothing -> return []
  
-- >>> sendRequest
-- The status code was: 200
-- Received {args = Object (fromList []), headers = Object (fromList [("Accept-Encoding",String "gzip"),("Host",String "httpbin.org"),("X-Amzn-Trace-Id",String "Root=1-657bd6b9-59411578215dcbe56f6212bb")]), origin = "24.43.123.86", url = "http://httpbin.org/get"}
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

