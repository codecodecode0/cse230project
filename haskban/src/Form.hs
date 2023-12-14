{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Form where

import Defs
import qualified Data.Text as T
-- import Data.Time
import Lens.Micro ((^.))
import Lens.Micro.TH

import Brick
import Brick.Forms
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

mkForm :: TaskFormData -> Form TaskFormData e ResourceName
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Title:" @@= editTextField name FormTitle (Just 1)
               , label "Description:" @@= editTextField desc FormDescription (Just 1)
              --  , label "Status" @@= radioField status [ (Todo,FormStatus, "Todo")
              --                                         , (InProgress,FormStatus, "InProgress")
              --                                         , (Completed,FormStatus, "Completed")
              --                                         ]
              --  , label "Due Date" @@= editShowableField dueDate FormDueDate
              --  , label "Assigned To" @@= editTextField assignedToId FormAssignedToId (Just 1)
               , label "Priority:" @@= radioField taskPriority [ (Low,FormPriority, "Low")
                                                          , (Medium,FormPriority, "Medium")
                                                          , (High,FormPriority, "High")
                                                          ]
               ]
