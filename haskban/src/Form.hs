{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Form(
    FormName(..),
    TaskStatus(..),
    TaskPriority(..),
    Task(..),
    mkForm,
    formDraw,
    formHandleEvent
) where

import qualified Data.Text as T
import Data.Time
import Lens.Micro ((^.))
import Lens.Micro.TH
-- import qualified Graphics.Vty as V
-- import Graphics.Vty.CrossPlatform (mkVty)

import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

--         title,
--         taskDescription = description,
--         taskOwnerId = ownerId,
--         taskStatus = status,
--         taskDueDate = dueDate,
--         taskAssignedToId = assignedToId,
--         taskPriority = priority,
data FormName = FormTitle
          | FormDescription
        --   | FormStatus
        --   | FormDueDate
        --   | FormAssignedToId
        --   | FormPriority
          deriving (Eq, Ord, Show)

data TaskStatus = Todo
                | InProgress
                | Completed
                deriving (Eq, Show)

data TaskPriority = Low
                  | Medium
                  | High
                  deriving (Eq, Show)

data Task = Task{
    _title :: T.Text,
    _description :: T.Text
    -- _status :: TaskStatus,
    -- _dueDate :: UTCTime,
    -- _assignedToId :: T.text,
    -- _priority :: TaskPriority
    } deriving (Show)

makeLenses ''Task

mkForm :: Task -> Form Task e FormName
mkForm = 
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Title" @@= editTextField title FormTitle (Just 1)
               , label "Description" @@= editTextField description FormDescription (Just 1)
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

-- theMap :: AttrMap
-- theMap = attrMap V.defAttr
--   [ (E.editAttr, V.white `on` V.black)
--   , (E.editFocusedAttr, V.black `on` V.yellow)
--   , (invalidFormInputAttr, V.white `on` V.red)
--   , (focusedFormInputAttr, V.black `on` V.yellow)
--   ]

formDraw :: Form Task e FormName -> [Widget FormName]
formDraw f =[C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Title is free-form text\n" <>
                     "- Description is free-form text\n" <>
                     "  Status is a radio field with 3 options\n" <>
                     "- Due Date is a time type field\n" <>
                     "- AssignedTo is a free-form text\n" <>
                     "- Priority is a radio field with 3 options\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

formHandleEvent = \ev -> do
            f <- gets formFocus
            case ev of
                VtyEvent (V.EvResize {}) -> return ()
                VtyEvent (V.EvKey V.KEsc []) -> halt
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent f /= Just FormAssignedToId -> halt
                _ -> do
                    handleFormEvent ev

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    -- st <- gets formState
                    -- modify $ setFieldValid (st^.age >= 18) AgeField


-- createForm :: IO (Form Task e FormName)
-- createForm = do
--     -- now <- getCurrentTime
--     let task = Task
--             { _title = ""
--             , _description = ""
--             -- , _status = Todo
--             -- , _dueDate = now
--             -- , _assignedToId = ""
--             -- , _priority = Low
--             }
--     f = mkForm task
--     return f