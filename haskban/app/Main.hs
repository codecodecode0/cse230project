module Main where

import           Brick
import Brick.Widgets.Border
import Data.Text.Internal as T
import           Graphics.Vty (defAttr)
import Defs
import Draw
import Events
import Form
import Data.Text (pack)
import Events (refreshBoard)

initBoard :: Board
initBoard = MkBoard
  { _todo = [TaskData (pack "T1") (pack "D1") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "T2") (pack "D2") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , _inProgress = [TaskData (pack "T3") (pack "D3") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , _done = [TaskData (pack "T4") (pack "D4") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low]
  , _pointer = [0, 0]
  }


-- Define the initial state
initialState :: AppState
initialState = MkAppState
  { _board = initBoard
  , _state = BoardState
  , _form = mkForm $ TaskFormData (pack "") (pack "") Low Todo (pack "") Nothing
  }

-- Define the app
app :: App AppState FormEvent ResourceName
app = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleApp
    , appStartEvent = refreshBoard
    , appAttrMap = const theMap
    }
    
-- Main function to run the app
main :: IO ()
main = defaultMain app initialState >> return ()