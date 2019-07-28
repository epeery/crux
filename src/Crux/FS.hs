{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Crux.FS
  ( File(..)
  , FS(..)
  , Session(..)
  , Stack(..)
  , TaskStatus(..)
  , emptyEntry
  , emptyFolder
  , emptyFS
  , emptyStack
  , emptyTask
  , fileBottom
  , fileDelete
  , fileDown
  , fileRename
  , fileSearch
  , fileTop
  , fileUp
  , fsDelete
  , fsIn
  , fsInsertFile
  , fsTop
  , fsOut
  , fsRename
  , insertFile
  , emptyNote
  , setPriority
  , stackFromList
  , sortFile
  , sortStack
  , stackToList
  , taskEnd
  , taskSetDone
  , taskSetTODO
  , taskStart
  , taskUnsetStatus
  ) where

import           Control.Monad ( mplus )

import           Data.List     ( sortOn )
import qualified Data.Text     as T
import           Data.Text     ( Text )
import           Data.Time

import           GHC.Generics  ( Generic )

------------------------------------------------------------------------
--                             Datatypes                              --
------------------------------------------------------------------------
--
-- A filesystem is a datatype consisting of the current folder
-- and a list of the previous folders visited
--
data FS = FS { fsCurrent :: File
             , fsPrev    :: [File] }
  deriving ( Show, Read, Eq, Generic )

--
-- A stack represents a directory. The first list of items are
-- the files above the focused file with their order reversed.
-- The middle item is the currently selected file, and last list
-- represents the files that are located below the focused file
--
data Stack = Stack { stackPrev    :: [File]
                   , stackCurrent :: File
                   , stackNext    :: [File] }
  deriving ( Show, Read, Eq, Generic )

--
-- A File is either an Empty file, an Entry, or a Folder containing a stack.
--
data File =
    Empty
  | Entry { name     :: Text
          , contents :: Stack
          , priority :: Int }
  | Folder { name     :: Text
           , contents :: Stack
           , priority :: Int }
  | Task { name     :: Text
         , contents :: Stack
         , dueDate  :: Maybe UTCTime
         , status   :: Maybe TaskStatus
         , sessions :: [Session]
         , priority :: Int }
  | Note { name     :: Text
         , noteDate :: UTCTime
         , priority :: Int }
  deriving ( Show, Read, Eq, Generic )

data TaskStatus = Tracking { startTime :: UTCTime }
                | Done { completedDate :: UTCTime }
                | TODO { todoDate :: UTCTime }
  deriving ( Show, Read, Eq, Generic )

data Session = Session { sessionStartDate :: UTCTime
                       , sessionEndDate   :: UTCTime }
  deriving ( Show, Read, Eq, Generic )

-- instance Show File where
--   show Empty = "Empty"
--   show (Folder n (Stack u c d) _) = show (n, Stack u c d)
--   show a = show $ name a
fileUp :: FS -> Maybe FS
fileUp (FS dir prev) = case fileUp' dir of
  Nothing -> Nothing
  Just f  -> Just (FS f prev)

fileUp' :: File -> Maybe File
fileUp' file = case u of
  []        -> Nothing
  (u' : us) -> Just $ file { contents = Stack us u' (f : ds) }
  where (Stack u f ds) = contents file

fileDown :: FS -> Maybe FS
fileDown (FS dir prev) = case fileDown' dir of
  Nothing -> Nothing
  Just f  -> Just (FS f prev)

fileDown' :: File -> Maybe File
fileDown' file = case d of
  []        -> Nothing
  (d' : ds) -> Just $ file { contents = Stack (f : u) d' ds }
  where (Stack u f d) = contents file

fsOut :: FS -> Maybe FS
fsOut (FS current (prev : ds)) = Just $
  FS (prev { contents = (contents prev) { stackCurrent = current } }) ds
fsOut _ = Nothing

fsIn :: FS -> Maybe FS
fsIn (FS f dirs) = case stackCurrent (contents f) of
  Empty  -> Nothing
  Note{} -> Nothing
  a      -> Just $ FS a (f : dirs)

fileTop :: FS -> Maybe FS
fileTop fs = case fileUp fs of
  Nothing -> Just fs
  Just f  -> fileTop f

fileBottom :: FS -> Maybe FS
fileBottom fs = case fileDown fs of
  Nothing -> Just fs
  Just f  -> fileBottom f

fsTop :: FS -> Maybe FS
fsTop (FS a []) = Just (FS a [])
fsTop fs        = fsOut fs >>= fsTop

fsInsertFile :: File -> FS -> Maybe FS
fsInsertFile f (FS folder dirs) = case insertFile f folder of
  Nothing -> Nothing
  Just a  -> Just $ FS a dirs

insertFile :: File -> File -> Maybe File
insertFile fileToInsert file = case stackCurrent (contents file) of
  Empty -> Just $ file { contents = Stack [] fileToInsert [] }
  c     ->
    if format fileToInsert < format c
    then Just $
      file { contents =
               (contents file) { stackPrev = reverse . sortFile $ fileToInsert
                                   : stackPrev (contents file) } }
    else Just $
      file { contents = (contents file) { stackNext = sortFile $ fileToInsert
                                            : stackNext (contents file) } }
  where format f = T.toLower (T.pack (show (priority f)) `T.append` " "
                              `T.append` name f)

fsDelete :: FS -> Maybe FS
fsDelete (FS file dirs) = case fileDelete file of
  Nothing -> Nothing
  Just a  -> Just $ FS a dirs

fileDelete :: File -> Maybe File
fileDelete Empty  = Nothing
fileDelete Note{} = Nothing
fileDelete file   = Just $
  file { contents = stackDeleteCurrent (contents file) }

fsRename :: FS -> Text -> Maybe FS
fsRename (FS Empty _) _   = Nothing
fsRename (FS Note{} _) _  = Nothing
fsRename (FS file prev) t = case fileRename (stackCurrent $ contents file) t of
  Nothing -> Nothing
  Just f  -> do
    let fs = FS file { contents = sortStack $
                         (contents file) { stackCurrent = f } }
                prev
    case fileSearch (name f) fs of
      Nothing  -> pure fs
      Just fs' -> pure fs'

fileRename :: File -> Text -> Maybe File
fileRename Empty _ = Nothing
fileRename f t     = Just $ f { name = t }

taskStart :: UTCTime -> FS -> Maybe FS
taskStart _ (FS Empty _)      = Nothing
taskStart _ (FS Note{} _)     = Nothing
taskStart time (FS file prev) =
  case taskStart' time . stackCurrent $ contents file of
    Nothing    -> Nothing
    Just file' -> Just $
      FS (file { contents = (contents file) { stackCurrent = file' } }) prev

taskStart' :: UTCTime -> File -> Maybe File
taskStart' time task@Task{} = Just $ task { status = Just $ Tracking time }
taskStart' _ _ = Nothing

taskEnd :: UTCTime -> FS -> Maybe FS
taskEnd _ (FS Empty _)      = Nothing
taskEnd _ (FS Note{} _)     = Nothing
taskEnd time (FS file prev) =
  case taskEnd' time . stackCurrent $ contents file of
    Nothing    -> Nothing
    Just file' -> Just $
      FS (file { contents = (contents file) { stackCurrent = file' } }) prev

taskEnd' :: UTCTime -> File -> Maybe File
taskEnd' time task@Task{} = case status task of
  Just (Tracking t) -> Just $ task { status   = Nothing
                                   , sessions = Session t time : sessions task }
  _ -> Nothing
taskEnd' _ _ = Nothing

setPriority :: Int -> FS -> Maybe FS
setPriority _ (FS Empty _)   = Nothing
setPriority n (FS file prev) =
  case setPriority' n . stackCurrent $ contents file of
    Nothing    -> Nothing
    Just file' -> do
      let fs = FS (file { contents = sortStack $
                            (contents file) { stackCurrent = file' } })
                  prev
      case fileSearch (name file') fs of
        Nothing  -> pure fs
        Just fs' -> pure fs'

setPriority' :: Int -> File -> Maybe File
setPriority' _ Empty = Nothing
setPriority' n file  = Just $ file { priority = n }

taskSetStatus :: (File -> Maybe File) -> FS -> Maybe FS
taskSetStatus _ (FS Empty _)   = Nothing
taskSetStatus _ (FS Note{} _)  = Nothing
taskSetStatus f (FS file prev) = case f . stackCurrent $ contents file of
  Nothing    -> Nothing
  Just file' -> do
    let fs = FS (file { contents = sortStack $
                          (contents file) { stackCurrent = file' } })
                prev
    case fileSearch (name file') fs of
      Nothing  -> pure fs
      Just fs' -> pure fs'

taskSetDone :: UTCTime -> FS -> Maybe FS
taskSetDone time = taskSetStatus (taskSetDone' time)

taskSetDone' :: UTCTime -> File -> Maybe File
taskSetDone' time file@Task{} = Just $ file { status   = Just $ Done time
                                            , priority = 10 }
taskSetDone' _ _ = Nothing

taskSetTODO :: UTCTime -> FS -> Maybe FS
taskSetTODO time = taskSetStatus (taskSetTODO' time)

taskSetTODO' :: UTCTime -> File -> Maybe File
taskSetTODO' time file@Task{} = Just $ file { status = Just $ TODO time }
taskSetTODO' _ _ = Nothing

taskUnsetStatus :: FS -> Maybe FS
taskUnsetStatus (FS Empty _)   = Nothing
taskUnsetStatus (FS Note{} _)  = Nothing
taskUnsetStatus (FS file prev) =
  case taskUnsetStatus' . stackCurrent $ contents file of
    Nothing    -> Nothing
    Just file' -> do
      let fs = FS (file { contents = sortStack $
                            (contents file) { stackCurrent = file' } })
                  prev
      case fileSearch (name file') fs of
        Nothing  -> pure fs
        Just fs' -> pure fs'

taskUnsetStatus' :: File -> Maybe File
taskUnsetStatus' file@Task{} = Just $ file { status = Nothing }
taskUnsetStatus' _           = Nothing

stackDeleteCurrent :: Stack -> Stack
stackDeleteCurrent (Stack _ Empty _)     = emptyStack
stackDeleteCurrent (Stack [] _ [])       = emptyStack
stackDeleteCurrent (Stack [] _ (d : ds)) = Stack [] d ds
stackDeleteCurrent (Stack (u : us) _ []) = Stack us u []
stackDeleteCurrent (Stack u _ (d : ds))  = Stack u d ds

sortFile :: [File] -> [File]
sortFile [ Empty ] = [ Empty ]
sortFile x         =
  sortOn (T.toLower <$> (\file -> T.pack (show (priority file)) `T.append` " "
                         `T.append` name file))
         x

sortStack :: Stack -> Stack
sortStack stack = stackFromList . sortFile $ stackToList stack

stackToList :: Stack -> [File]
stackToList (Stack u c d) = reverse u ++ c : d

stackFromList :: [File] -> Stack
stackFromList []       = emptyStack
stackFromList (x : xs) = Stack [] x xs

fileSearch :: Text -> FS -> Maybe FS
fileSearch _ (FS Empty _)   = Nothing
fileSearch n (FS file prev) = case fileSearch' n file of
  Nothing    -> Nothing
  Just file' -> Just $ FS file' prev

fileSearch' :: Text -> File -> Maybe File
fileSearch' _ Empty = Nothing
fileSearch' _ Note{} = Nothing
fileSearch' n container = case stackCurrent $ contents container of
  Empty -> Nothing
  file  -> if T.toLower n `T.isInfixOf` T.toLower (name file)
           then Just container
           else lookNext container `mplus` lookPrev container
  where lookPrev       = look fileUp'

        lookNext       = look fileDown'

        look func nec_ = do
          nec' <- func nec_
          case stackCurrent $ contents nec' of
            Empty -> Nothing
            file' -> if T.toLower n `T.isInfixOf` T.toLower (name file')
                     then Just nec'
                     else look func nec'

emptyStack :: Stack
emptyStack = Stack [] Empty []

emptyEntry :: Text -> File
emptyEntry n = Entry n emptyStack 0

emptyFolder :: Text -> File
emptyFolder n = Folder n emptyStack 0

emptyTask :: Text -> File
emptyTask n = Task { name     = n
                   , contents = emptyStack
                   , dueDate  = Nothing
                   , status   = Nothing
                   , sessions = []
                   , priority = 0 }

emptyFS :: Text -> FS
emptyFS n = FS (emptyFolder n) []

emptyNote :: Text -> UTCTime -> File
emptyNote n date = Note { name     = n
                        , noteDate = date
                        , priority = 0 }
