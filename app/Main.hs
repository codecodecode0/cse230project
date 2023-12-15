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
  { _todo = [TaskData (pack "CSE 230 Project") (pack "HaskTask- Networked TashBoard") Todo (read "2019-01-01 00:00:00 UTC") (pack "") Low, TaskData (pack "ECE 257A Project") (pack "Using LeoEM for satellite network simulations") Todo (read "2019-01-01 00:00:00 UTC") (pack "Siddhant") Low]
  , _inProgress = [TaskData (pack "CSE 258 Assignment 2") (pack "Predicting Usefulness of Amazon review") InProgress (read "2019-01-01 00:00:00 UTC") (pack "Shril") Low]
  , _done = [TaskData (pack "ECE 277 Project") (pack "Efficient road finding algorithm") Completed (read "2019-01-01 00:00:00 UTC") (pack "Viraj") Low]
  , _pointer = [0, 0]
  }

emptyBoard :: Board
emptyBoard = MkBoard
  { _todo = []
  , _inProgress = []
  , _done = []
  , _pointer = [0, 0]
}

-- Define the initial state
initialState :: AppState
initialState = MkAppState
  { _board = initBoard,
  _state = BoardState,
  _form = mkForm $ TaskData (pack "") (pack "") Todo Nothing (pack "") Low,
  _fullBoardCopy = initBoard,
  _filterForm = mkFilterForm $ FilterFormData (pack ""),
  _filteredBoard = emptyBoard
  }

-- Define the app
app :: App AppState FormEvent ResourceName
app = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleApp
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }
    
-- Main function to run the app
main :: IO ()
main = defaultMain app initialState >> return ()