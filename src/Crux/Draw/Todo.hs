{-# LANGUAGE OverloadedStrings #-}

module Crux.Draw.Todo where

import           Brick.Types          as B
import           Brick.Widgets.Border as B
import           Brick.Widgets.Center as B
import           Brick.Widgets.Core   as B

import           Crux.Bindings        ()
import           Crux.Browser
import           Crux.Core
import           Crux.FS
import           Crux.Style

import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           Data.Text            ( Text )

renderTodo :: File -> Widget ResourceName
renderTodo = center . withAttr entry . borderWithLabel (txt "Today's TODOs")
  . padAll 2 . drawTodoCursor

drawTodoCursor :: File -> Widget ResourceName
drawTodoCursor file = dir
  where (Stack u c d) = contents file

        dir           = vBox $
          mconcat [ map (drawTodo False) $ reverse u
                  , [ visible $ vLimit 1 $ drawTodo True c ]
                  , map (drawTodo False) d ]

drawTodo :: Bool -> File -> Widget n
drawTodo _ Empty = withAttr empty $ txt "No TODOs yet"
drawTodo selected file@Task{} =
  let doneStyle b = case status file of
        Just Done{} -> if b
                       then forceAttr selectedDoneTodo
                       else forceAttr doneTodo
        _           -> id
  in
      if selected
      then doneStyle True . withAttr selectedEntry . txt $ name file
      else doneStyle False . withAttr entry . txt $ name file
drawTodo _ _ = emptyWidget
