{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Actions.Browser where

import           Crux.Actions.Utils
import           Crux.Browser
import           Crux.Core
import           Crux.FS

import qualified Data.Maybe         as M
import qualified Data.Text          as T
import           Data.Text          ( Text )
import           Data.Time

browserUp :: Action
browserUp = Action { actionName = "Up"
                   , actionDesc = "Move up"
                   , actionFunc = modifyBrowserCursor fileUp }

browserDown :: Action
browserDown = Action { actionName = "Down"
                     , actionDesc = "Move down"
                     , actionFunc = modifyBrowserCursor fileDown }

todoUp :: Action
todoUp = Action { actionName = "Up"
                , actionDesc = "Move up"
                , actionFunc = modifyTodoCursor fileUp' }

todoDown :: Action
todoDown = Action { actionName = "Down"
                  , actionDesc = "Move down"
                  , actionFunc = modifyTodoCursor fileDown' }

todoOpen :: Action
todoOpen = Action { actionName = "Open TODO item"
                  , actionDesc = "Go to the currently selected TODO item"
                  , actionFunc = do
                      p <- stackCurrent . contents . cruxTodos <$> get
                      case p of
                        Task{} -> case path p of
                          Nothing    -> liftIO $ print "Nothing."
                          Just path' -> modifyBrowserCursor (goToFSPath path')
                        _      -> pure ()
                      modifyActiveView Browser }

browserOpen :: Action
browserOpen = Action { actionName = "Open"
                     , actionDesc = "Open a file"
                     , actionFunc = modifyBrowserCursor fsIn }

browserBack :: Action
browserBack = Action { actionName = "Back"
                     , actionDesc = "Move back to the previous directory"
                     , actionFunc = modifyBrowserCursor fsOut }

getPath :: Action
getPath = Action { actionName = "Back"
                 , actionDesc = "Move back to the previous directory"
                 , actionFunc = do
                     fs <- browserCursor . cruxBrowserState <$> get
                     liftIO . print $ getFSPath fs }

browserTop :: Action
browserTop = Action { actionName = "Top"
                    , actionDesc = "Move to the top level directory"
                    , actionFunc = modifyBrowserCursor fsTop }

browserHigh :: Action
browserHigh = Action { actionName = "High"
                     , actionDesc = "Move to the top of the current directory"
                     , actionFunc = modifyBrowserCursor fileTop }

browserGHigh :: Action
browserGHigh = Action { actionName = "Go to top"
                      , actionDesc = "Move to the top of the current directory"
                      , actionFunc = modifyBrowserCursor fileTop
                          *> modifyBrowserMode BrowserNormal }

browserLow :: Action
browserLow = Action { actionName = "Low"
                    , actionDesc = "Move to the bottom of the current directory"
                    , actionFunc = modifyBrowserCursor fileBottom }

newFolder :: Action
newFolder = Action { actionName = "New folder"
                   , actionDesc = "Create a new folder"
                   , actionFunc = folderInsert }

newEntry :: Action
newEntry = Action { actionName = "New entry"
                  , actionDesc = "Create a new entry "
                  , actionFunc = modifyBrowserMode (BrowserInsert BIEntry "") }

newTask :: Action
newTask = Action { actionName = "New Task"
                 , actionDesc = "Create a new task"
                 , actionFunc = modifyBrowserMode (BrowserInsert BITask "") }

newNote :: Action
newNote = Action { actionName = "New entry"
                 , actionDesc = "Create a new entry or task"
                 , actionFunc = modifyBrowserMode (BrowserInsert BINote "") }

browserDelete :: Action
browserDelete = Action { actionName = "Delete"
                       , actionDesc = "Delete the currently selected file"
                       , actionFunc = modifyBrowserCursor fsDelete
                           *> modifyBrowserMode BrowserNormal }

browserRename :: Action
browserRename = Action { actionName = "Rename"
                       , actionDesc = "Rename the currently selected file"
                       , actionFunc = renameInsert }

browserSearch :: Action
browserSearch =
  Action { actionName = "Search"
         , actionDesc = "Search for a file in the current directory"
         , actionFunc = modifyBrowserMode (BrowserInsert BISearch "") }

browserTrackToggle :: Action
browserTrackToggle =
  Action { actionName = "Toggle task tracking"
         , actionDesc = "Toggle tracking time for selected task"
         , actionFunc = trackToggle }

browserPriorityMode :: Action
browserPriorityMode =
  Action { actionName = "Set priority"
         , actionDesc = "Switch to priority mode"
         , actionFunc = modifyBrowserMode (BrowserCommand BCPriority) }

priority0 :: Action
priority0 = Action { actionName = "Default priority"
                   , actionDesc = "Set the selected file's priority to normal"
                   , actionFunc = modifyBrowserCursor (setPriority 0)
                       *> modifyBrowserMode BrowserNormal }

priority1 :: Action
priority1 = Action { actionName = "Priority 1"
                   , actionDesc = "Set the selected file's priority to 1"
                   , actionFunc = modifyBrowserCursor (setPriority (-1))
                       *> modifyBrowserMode BrowserNormal }

priority2 :: Action
priority2 = Action { actionName = "Priority 2"
                   , actionDesc = "Set the selected file's priority to 2"
                   , actionFunc = modifyBrowserCursor (setPriority (-2))
                       *> modifyBrowserMode BrowserNormal }

priority3 :: Action
priority3 = Action { actionName = "Priority 3"
                   , actionDesc = "Set the selected file's priority to 3"
                   , actionFunc = modifyBrowserCursor (setPriority (-3))
                       *> modifyBrowserMode BrowserNormal }

priority4 :: Action
priority4 = Action { actionName = "Priority 4"
                   , actionDesc = "Set the selected file's priority to 4"
                   , actionFunc = modifyBrowserCursor (setPriority (-4))
                       *> modifyBrowserMode BrowserNormal }

priority5 :: Action
priority5 = Action { actionName = "Priority 5"
                   , actionDesc = "Set the selected file's priority to 5"
                   , actionFunc = modifyBrowserCursor (setPriority (-5))
                       *> modifyBrowserMode BrowserNormal }

priority6 :: Action
priority6 = Action { actionName = "Priority 6"
                   , actionDesc = "Set the selected file's priority to 6"
                   , actionFunc = modifyBrowserCursor (setPriority (-6))
                       *> modifyBrowserMode BrowserNormal }

priority7 :: Action
priority7 = Action { actionName = "Priority 7"
                   , actionDesc = "Set the selected file's priority to 7"
                   , actionFunc = modifyBrowserCursor (setPriority (-7))
                       *> modifyBrowserMode BrowserNormal }

priority8 :: Action
priority8 = Action { actionName = "Priority 8"
                   , actionDesc = "Set the selected file's priority to 8"
                   , actionFunc = modifyBrowserCursor (setPriority (-8))
                       *> modifyBrowserMode BrowserNormal }

priority9 :: Action
priority9 = Action { actionName = "Priority 9"
                   , actionDesc = "Set the selected file's priority to 9"
                   , actionFunc = modifyBrowserCursor (setPriority (-9))
                       *> modifyBrowserMode BrowserNormal }

trackToggle :: Crux ()
trackToggle = do
  now <- liftIO getCurrentTime
  modifyBrowserCursor (\fs -> case fsCurrent fs of
                         Empty     -> Nothing
                         Note{}    -> Nothing
                         container -> case stackCurrent $ contents container of
                           Task{status = Nothing} -> taskStart now fs
                           Task{status = Just _} -> taskEnd now fs
                           _ -> Nothing)
  modifyBrowserMode BrowserNormal

browserNormalMode :: Action
browserNormalMode = Action { actionName = "Normal mode"
                           , actionDesc = "Switch to Normal mode"
                           , actionFunc = modifyBrowserMode BrowserNormal }

browserCreateMode :: Action
browserCreateMode = Action { actionName = "Create mode"
                           , actionDesc = "Switch to Create mode"
                           , actionFunc = selectCreateMode }

browserDeleteMode :: Action
browserDeleteMode =
  Action { actionName = "Create mode"
         , actionDesc = "Switch to Create mode"
         , actionFunc = modifyBrowserMode (BrowserCommand BCDelete) }

browserTaskMode :: Action
browserTaskMode = Action { actionName = "Task mode"
                         , actionDesc = "Switch to Task mode"
                         , actionFunc = selectTaskMode }

browserGMode :: Action
browserGMode = Action { actionName = "g mode"
                      , actionDesc = "Switch to 'g' mode"
                      , actionFunc = modifyBrowserMode (BrowserCommand BCG) }

browserInsertAppend :: Action
browserInsertAppend = Action { actionName = "Handle insert"
                             , actionDesc = "Type while in insert more"
                             , actionFunc = browserInsert }

---------------------------
--  TODO: Refactor this  --
---------------------------
toggleTaskTODO :: Action
toggleTaskTODO =
  Action { actionName = "Toggle TODO"
         , actionDesc = "Toggle the currently selected task's 'TODO status"
         , actionFunc = do
             now <- liftIO getCurrentTime
             fs <- browserCursor . cruxBrowserState <$> get
             case fsCurrent fs of
               Empty     -> pure ()
               Note{}    -> pure ()
               container -> case stackCurrent $ contents container of
                 Task{todoDate = Nothing} -> do
                   modify (\st ->
                           st { cruxTodos =
                                  case (insertFile ((stackCurrent . contents $
                                                     fsCurrent fs) { path = Just $ getFSPath fs })
                                                   (cruxTodos st)) of
                                    Nothing -> cruxTodos st
                                    Just a  -> a })
                   modifyBrowserCursor $ taskSetTODO now
                 Task{todoDate = Just _} -> do
                   modify (\st ->
                           st { cruxTodos =
                                  (cruxTodos st) { contents = stackFromList $
                                                     filter ((/= (Just $
                                                                  getFSPath fs))
                                                             . path)
                                                            (stackToList $
                                                             contents $
                                                             cruxTodos st) } })
                   modifyBrowserCursor taskUnsetTODO
                 _ -> pure ()
             modifyBrowserMode BrowserNormal }

---------------------------
--  TODO: Refactor this  --
---------------------------
toggleTaskDone :: Action
toggleTaskDone =
  Action { actionName = "Toggle done"
         , actionDesc = "Toggle the currently selected task's 'Done' status"
         , actionFunc = do
             now <- liftIO getCurrentTime
             modifyBrowserCursor (\fs -> case fsCurrent fs of
                                    Empty     -> Nothing
                                    Note{}    -> Nothing
                                    container ->
                                      case stackCurrent $ contents container of
                                        Task{ status = Nothing
                                            } -> case taskSetDone now fs of
                                          Nothing  -> Just fs
                                          Just fs' -> setPriority 10 fs'
                                        Task{ status = Just Done{}
                                            } -> case taskUnsetStatus fs of
                                          Nothing  -> Just fs
                                          Just fs' -> setPriority 0 fs'
                                        _ -> Nothing)
             modifyBrowserMode BrowserNormal }

handleInsertEnter :: BrowserMode -> Crux ()
handleInsertEnter BrowserNormal = pure ()
handleInsertEnter (BrowserInsert BIRename t) = do
  modifyBrowserCursor (`fsRename` t)
  modifyBrowserMode BrowserNormal
handleInsertEnter (BrowserInsert BISearch t) = do
  modifyBrowserCursor (fileSearch t)
  modifyBrowserMode BrowserNormal
handleInsertEnter f = do
  date <- liftIO getCurrentTime
  let file = case f of
        BrowserInsert BIFolder t -> Just $ emptyFolder t
        BrowserInsert BIEntry t -> Just $ emptyEntry t
        BrowserInsert BITask t -> Just $ emptyTask t
        BrowserInsert BINote t -> Just $ emptyNote t date
        _ -> Nothing
  case file of
    Just file' -> modifyBrowserCursor (\fs -> case fileSearch (name file') fs of
                                         Nothing  -> fsInsertFile file' fs
                                         Just fs' -> Just fs')
    Nothing    -> pure ()
  modifyBrowserMode BrowserNormal

browserInsert :: Crux ()
browserInsert = do
  CruxState{..} <- get
  let BrowserState{..} = cruxBrowserState
  case key of
    Nothing -> pure ()
    Just k  -> case browserMode of
      BrowserInsert m t -> handleBrowserInsert k m t
      _ -> pure ()

handleBrowserInsert :: Binding -> BInsert -> Text -> Crux ()
handleBrowserInsert (BChar c) m t =
  modifyBrowserInsert m (t `T.append` T.pack [ c ])
handleBrowserInsert (BKey "Space") m t =
  modifyBrowserInsert m (t `T.append` " ")
handleBrowserInsert (BKey "Tab") m t =
  modifyBrowserInsert m (t `T.append` "  ")
handleBrowserInsert (BKey "Backspace") m t =
  modifyBrowserInsert m (T.dropEnd 1 t)
handleBrowserInsert (BKey "Enter") m t = modifyBrowserInsert m (T.dropEnd 1 t)
  *> handleInsertEnter (BrowserInsert m t)
handleBrowserInsert (BKey "Escape") _ _ =
  modify (\s -> s { cruxBrowserState =
                      (cruxBrowserState s) { browserMode = BrowserNormal } })
handleBrowserInsert _ _ _ = pure ()

folderInsert :: Crux ()
folderInsert = do
  file <- fsCurrent . browserCursor . cruxBrowserState <$> get
  case file of
    Folder{} -> modifyBrowserMode (BrowserInsert BIFolder "")
    _        -> pure ()

renameInsert :: Crux ()
renameInsert = do
  file <- stackCurrent . contents . fsCurrent . browserCursor . cruxBrowserState
    <$> get
  case file of
    Empty -> pure ()
    f     -> modifyBrowserMode (BrowserInsert BIRename (name f))

selectCreateMode :: Crux ()
selectCreateMode = do
  CruxState{..} <- get
  let BrowserState{..} = cruxBrowserState
      file = fsCurrent browserCursor
  case file of
    Folder{..} -> modifyBrowserMode (BrowserCommand BCFolderCreate)
    Entry{..}  -> modifyBrowserMode (BrowserCommand BCEntryCreate)
    Task{..}   -> modifyBrowserMode (BrowserCommand BCTaskCreate)
    _          -> pure ()

selectTaskMode :: Crux ()
selectTaskMode = do
  CruxState{..} <- get
  let BrowserState{..} = cruxBrowserState
      file = fsCurrent browserCursor
  case file of
    Entry{..} -> modifyBrowserMode (BrowserCommand BCTask)
    _         -> pure ()
