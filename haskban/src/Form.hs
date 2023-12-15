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

mkForm :: TaskFormData -> Form TaskFormData e ResourceName
mkForm =
    let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Title:" @@= editTextField nameForm FormTitle (Just 1)
               , label "Description:" @@= editTextField descForm FormDescription (Just 1)
              --  , label "Due Date" @@= editShowableField dueDateForm FormDueDate
               , label "Assigned To" @@= editTextField assignedToIdForm FormAssignedToId (Just 1)
               , label "Status" @@= radioField statusForm [ (Todo,FormStatus, "Todo")
                                                      , (InProgress,FormStatus, "InProgress")
                                                      , (Completed,FormStatus, "Completed")
                                                      ]
               , label "Priority:" @@= radioField taskPriorityForm [ (Low,FormPriority, "Low")
                                                          , (Medium,FormPriority, "Medium")
                                                          , (High,FormPriority, "High")
                                                          ]
               ]
      -- where
      --   validDate date = case date of
      --     Nothing -> False
      --     Just d -> d > read "2019-01-01 00:00:00 UTC"


-- mkForm :: TaskFormData -> Form TaskFormData e ResourceName
-- mkForm = 
--   newForm [
--     (str "Name: " <+>) @@= 
--       editTextField nameForm FormTitle (Just 1),
--     (str "Description: " <+>) @@=
--       editTextField descForm FormDescription (Just 1),
--     (str "Priority: " <+>) @@=
--       radioField taskPriorityForm [ (Low,FormPriority, "Low")
--                               , (Medium,FormPriority, "Medium")
--                               , (High,FormPriority, "High")
--                               ],
--     (str "Status: " <+>) @@=
--       radioField statusForm [ (Todo,FormStatus, "Todo")
--                         , (InProgress,FormStatus, "InProgress")
--                         , (Completed,FormStatus, "Completed")
--                         ],
--     (str "Assigned To: " <+>) @@=
--       editTextField assignedToIdForm FormAssignedToId (Just 1)
--   ]

mkFilterForm :: FilterFormData -> Form FilterFormData e ResourceName
mkFilterForm = 
  let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "FilterTasksby:" @@= editTextField filterAssignedToIdForm FormAssignedToId (Just 1)]