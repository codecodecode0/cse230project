module Lib
    ( someFunc
    ) where
import Server.Server ( serverMain )

someFunc :: IO ()
someFunc = serverMain
