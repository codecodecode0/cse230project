{-# LANGUAGE OverloadedStrings #-}
module UIComponents where

import Brick
import Brick.Widgets.Border
import Form
import Lens.Micro ((^.))

drawTask :: Task -> Widget n
drawTask task = vBox
    [ hBox [str "Title: ", txt $ task^.title]
    , hBox [str "Description: ", txtWrap $ task^.description]
    , hBorder
    ]

drawBoard :: [Task] -> Widget n
drawBoard tasks = vBox $ map drawTask tasks
