module Main where

import           Brick
import Brick.Widgets.Border
import Data.Text.Internal as T
-- import           Brick.Widgets.Panes
import           Graphics.Vty (defAttr)
import Defs
import Draw
import Events
-- import Form
-- import qualified Graphics.Vty as Vty
import Data.Text (pack)

initBoard :: Board
initBoard = Board
  { todo = [TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , inProgress = [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , done = [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , pointer = [0, 0]
  }
-- Define the initial state
initialState :: AppState
initialState = TaskBoard initBoard

-- Define the app
app :: App AppState e ResourceName
app = App
    { appDraw = drawApp
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleApp
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }
    
-- Main function to run the app
main :: IO ()
main = defaultMain app initialState >> return ()