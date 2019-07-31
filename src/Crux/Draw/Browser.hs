{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Draw.Browser ( renderBrowser ) where

import           Brick.Types          as B
import           Brick.Widgets.Border as B
import           Brick.Widgets.Core   as B

import           Crux.Bindings        ()
import           Crux.Browser
import           Crux.Core
import           Crux.FS
import           Crux.Style

import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import           Data.Text            ( Text )

renderBrowser :: CruxConfig -> BrowserState -> Widget ResourceName
renderBrowser conf BrowserState{..} =
  drawBrowser browserCursor <=> drawMenu conf browserMode

drawBrowser :: FS -> Widget ResourceName
drawBrowser fs = padLeftRight 1 $ current <+> pad next
  where current =
          viewport "browser-current" Vertical $ pad $ drawBrowserCursor fs

        next    = viewport "browser-next" Vertical $ pad $ drawBrowserNext fs

        pad     = padLeft (Pad 1) . withAttr project

drawBrowserCursor :: FS -> Widget ResourceName
drawBrowserCursor (FS cur _) = dir
  where (Stack u c d) = contents cur

        dir           = vBox $
          mconcat [ map (drawPath False) $ reverse u
                  , [ visible $ vLimit 1 $ drawPath True c ]
                  , map (drawPath False) d ]

drawBrowserNext :: FS -> Widget ResourceName
drawBrowserNext (FS cur _) = dir
  where (Stack _ c _) = contents cur

        dir           = case c of
          Empty  -> emptyWidget
          Note{} -> emptyWidget
          f      -> drawBrowserCursor (FS f [])

drawPath :: Bool -> File -> Widget n
drawPath True file = case file of
  Empty       -> selectedStyle empty "Empty"
  Folder{..}  -> selectedStyle selectedFolder name
  Project{..} -> selectedStyle selectedProject name
  Task{..}    ->
    let todoStyle = case todoDate of
          Nothing -> selectedStyle selectedTask
          Just _  -> selectedStyle selectedTodo . ("٭ " `T.append`)
    in
        todoStyle (taskStatusText status `T.append` showPriority priority
                   `T.append` name)
  Note{..}    -> selectedStyle selectedNote name
  where selectedStyle s n = withAttr s $ txt (normalizeText n) <+> fill ' '
drawPath _ file = case file of
  Empty       -> unselectedStyle empty "Empty"
  Folder{..}  -> unselectedStyle folder name
  Project{..} -> unselectedStyle project name
  Task{..}    ->
    let todoStyle = case todoDate of
          Nothing -> unselectedStyle task
          Just _  -> unselectedStyle todo . ("٭ " `T.append`)
    in
        todoStyle (taskStatusText status `T.append` showPriority priority
                   `T.append` name)
  Note{..}    -> unselectedStyle note name
  where unselectedStyle s n = withAttr s . txt $ normalizeText n

showPriority :: Int -> Text
showPriority n
  | n >= 0 = ""
  | otherwise = T.pack $ show (n * (-1)) ++ ": "

taskStatusText :: Maybe TaskStatus -> Text
taskStatusText Nothing           = ""
taskStatusText (Just Tracking{}) = "[STARTED] "
taskStatusText (Just Done{})     = "[DONE] "

normalizeText :: Text -> Text
normalizeText "" = " "
normalizeText s  = s

drawMenu :: CruxConfig -> BrowserMode -> Widget ResourceName
drawMenu CruxConfig{..} mode = case mode of
  BrowserInsert{} -> drawInsertInput mode
  BrowserCommand c -> case c of
    BCFolderCreate -> drawKeybindsMenu browserFolderCreateBindings
    BCProjectCreate -> drawKeybindsMenu browserProjectCreateBindings
    BCTaskCreate -> drawKeybindsMenu browserTaskCreateBindings
    BCDelete -> drawKeybindsMenu browserDeleteBindings
    BCPriority -> drawKeybindsMenu browserPriorityBindings
    BCTask -> drawKeybindsMenu browserTaskBindings
    BCG -> drawKeybindsMenu browserGBindings
  _ -> emptyWidget
  where ViewBindings{..} = configBindings

drawKeybindsMenu :: Bindings -> Widget ResourceName
drawKeybindsMenu bindsMap = case M.toList bindsMap of
  [] -> emptyWidget
  bindsList -> border $
    hBox [ vBox $ hLimit 10 . vLimit 1 . (<+> fill ' ') . str . show <$> binds
         , vBox $ vLimit 1 . (<+> fill ' ') . str . show <$> actions ]
    where (binds, actions) = foldr (\(b, a) (bs, as) -> case b of
                                      BAll -> (bs, as)
                                      _    -> (b : bs, a : as))
                                   ([], [])
                                   bindsList

drawInsertInput :: BrowserMode -> Widget ResourceName
drawInsertInput mode = withAttr folder (txt modeText)
  where modeText = case mode of
          BrowserInsert BIFolder t -> "New folder: " `T.append` t `T.append` "|"
          BrowserInsert BIProject t ->
            "New project: " `T.append` t `T.append` "|"
          BrowserInsert BITask t -> "New task: " `T.append` t `T.append` "|"
          BrowserInsert BINote t -> "New note: " `T.append` t `T.append` "|"
          BrowserInsert BIRename t -> "Rename: " `T.append` t `T.append` "|"
          BrowserInsert BISearch t -> "/" `T.append` t `T.append` "|"
          _ -> ""
