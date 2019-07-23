{-# LANGUAGE OverloadedStrings #-}

module Crux.Style where

import           Brick.AttrMap           as B
import           Brick.Util              as B

import qualified Graphics.Vty            as V
import           Graphics.Vty.Attributes

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
  attrMap mempty
          [ ( selectedFolder
              , fg V.brightBlue `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , ( selectedEntry
              , fg V.brightWhite `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , ( selectedTask
              , fg V.brightGreen `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , ( selectedNote
              , fg V.brightCyan `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , (folder, fg V.brightBlue `V.withStyle` V.bold)
          , (entry, fg V.white)
          , (task, fg V.brightGreen)
          , (note, fg V.brightCyan)
          , (empty, fg V.brightRed) ]

selectedAttr :: AttrName
selectedAttr = "selected"

empty :: AttrName
empty = "empty"

folder :: AttrName
folder = "folder"

selectedFolder :: AttrName
selectedFolder = folder <> "selected"

entry :: AttrName
entry = "file"

selectedEntry :: AttrName
selectedEntry = entry <> "selected"

task :: AttrName
task = "task"

selectedTask :: AttrName
selectedTask = task <> "selected"

note :: AttrName
note = "note"

selectedNote :: AttrName
selectedNote = note <> "selected"
