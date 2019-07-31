{-# LANGUAGE OverloadedStrings #-}

module Crux.Style where

import           Brick.AttrMap as B
import           Brick.Util    as B

import qualified Graphics.Vty  as V

defaultAttrMap :: s -> AttrMap
defaultAttrMap _ =
  attrMap mempty
          [ ( selectedFolder
              , fg V.brightBlue `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , ( selectedEntry
              , V.defAttr `V.withStyle` V.bold `V.withStyle` V.reverseVideo
              )
          , ( selectedTask
              , fg V.brightGreen `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , ( selectedTodo
              , fg V.brightYellow `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo `V.withStyle` V.underline
              )
          , ( selectedDoneTodo
              , fg V.white `V.withStyle` V.reverseVideo `V.withStyle` V.bold
              )
          , ( selectedNote
              , fg V.brightCyan `V.withStyle` V.bold
                `V.withStyle` V.reverseVideo
              )
          , (folder, fg V.brightBlue `V.withStyle` V.bold)
          , (entry, V.defAttr)
          , (task, fg V.brightGreen)
          , (doneTodo, fg V.white `V.withStyle` V.dim)
          , (note, fg V.brightCyan)
          , (todo, fg V.brightYellow `V.withStyle` V.underline)
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

todo :: AttrName
todo = "todo"

selectedTodo :: AttrName
selectedTodo = todo <> "selected"

doneTodo :: AttrName
doneTodo = todo <> "done"

selectedDoneTodo :: AttrName
selectedDoneTodo = doneTodo <> "selected"

note :: AttrName
note = "note"

selectedNote :: AttrName
selectedNote = note <> "selected"
