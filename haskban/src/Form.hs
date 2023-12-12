{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Form where

import Brick.Forms
import Lens.Micro.TH
import qualified Data.Text as T
-- import Data.Time
-- import Lens.Micro ((^.))
import Lens.Micro.TH
-- import qualified Graphics.Vty as V
-- import Graphics.Vty.CrossPlatform (mkVty)

import Brick
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
-- import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Task = Task {
    _title :: T.Text,
    _description :: T.Text
} deriving (Show)

makeLenses ''Task

data FormName = FormTitle | FormDescription deriving (Eq, Ord, Show)

mkForm :: Task -> Form Task e FormName
mkForm = 
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Title:" @@= editTextField title FormTitle (Just 1)
               , label "Description:" @@= editTextField description FormDescription (Just 1)
            --    , label "Status" @@= radioField status [ (Todo, "Todo")
            --                                           , (InProgress, "InProgress")
            --                                           , (Completed, "Completed")
            --                                           ]
            --    , label "Due Date" @@= editShowableField dueDate FormDueDate
            --    , label "Assigned To" @@= editTextField assignedToId FormAssignedToId (Just 1)
            --    , label "Priority" @@= radioField priority [ (Low, "Low")
            --                                               , (Medium, "Medium")
            --                                               , (High, "High")
            --                                               ]
               ]
