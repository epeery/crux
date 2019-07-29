{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Crux.FS
  ( File(..)
  , FileStack(..)
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
  , getFSPath
  , goToFSPath
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
  , taskUnsetTODO
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
data Stack a = Stack { stackPrev    :: [a]
                     , stackCurrent :: a
                     , stackNext    :: [a] }
  deriving ( Show, Read, Eq, Generic )

instance Functor Stack where
  fmap f (Stack u c d) = Stack (fmap f u) (f c) (fmap f d)

instance Foldable Stack where
  foldMap f = foldMap f . stackToList

newtype FileStack = FileStack { getFileStack :: Stack File }
  deriving ( Show, Read, Eq, Generic )

--
-- A File is either an Empty file, an Entry, or a Folder containing a stack.
--
data File =
    Empty
  | Entry { name     :: Text
          , contents :: FileStack
          , priority :: Int }
  | Folder { name     :: Text
           , contents :: FileStack
           , priority :: Int }
  | Task { name     :: Text
         , contents :: FileStack
         , dueDate  :: Maybe UTCTime
         , status   :: Maybe TaskStatus
         , todoDate :: Maybe UTCTime
         , sessions :: [Session]
         , priority :: Int }
  | Note { name     :: Text
         , noteDate :: UTCTime
         , priority :: Int }
  deriving ( Show, Read, Eq, Generic )

data TaskStatus = Tracking { startTime :: UTCTime }
                | Done { completedDate :: UTCTime }
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
  (u' : us) -> Just $ file { contents = FileStack $ Stack us u' (f : ds) }
  where (Stack u f ds) = getFileStack $ contents file

fileDown :: FS -> Maybe FS
fileDown (FS dir prev) = case fileDown' dir of
  Nothing -> Nothing
  Just f  -> Just (FS f prev)

fileDown' :: File -> Maybe File
fileDown' file = case d of
  []        -> Nothing
  (d' : ds) -> Just $ file { contents = FileStack $ Stack (f : u) d' ds }
  where (Stack u f d) = getFileStack $ contents file

fsOut :: FS -> Maybe FS
fsOut (FS current (prev : ds)) = Just $
  FS (prev { contents = FileStack $
               (getFileStack $ contents prev) { stackCurrent = current } })
     ds
fsOut _ = Nothing

fsIn :: FS -> Maybe FS
fsIn (FS f dirs) = case stackCurrent (getFileStack $ contents f) of
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
insertFile fileToInsert
           file = case stackCurrent (getFileStack $ contents file) of
  Empty -> Just $ file { contents = FileStack $ Stack [] fileToInsert [] }
  c     ->
    if format fileToInsert < format c
    then Just $
      file { contents = FileStack $
               (getFileStack $ contents file) { stackPrev = reverse . sortFile $
                                                  fileToInsert
                                                  : stackPrev (getFileStack $
                                                               contents file) } }
    else Just $
      file { contents = FileStack $
               (getFileStack $ contents file) { stackNext =
                                                  sortFile $ fileToInsert
                                                  : stackNext (getFileStack $
                                                               contents file) } }
  where format f = T.toLower (T.pack (show (priority f)) `T.append` " "
                              `T.append` name f)

fsDelete :: FS -> Maybe FS
fsDelete (FS file dirs) = case fileDelete file of
  Nothing -> Nothing
  Just a  -> Just $ FS a dirs

fileDelete :: File -> Maybe File
fileDelete Empty  = Nothing
fileDelete Note{} = Nothing
fileDelete file   = case stackDeleteCurrent (getFileStack $ contents file) of
  Nothing    -> Nothing
  Just file' -> Just $ file { contents = FileStack file' }

fsRename :: FS -> Text -> Maybe FS
fsRename (FS Empty _) _   = Nothing
fsRename (FS Note{} _) _  = Nothing
fsRename (FS file prev) t =
  case fileRename (stackCurrent . getFileStack $ contents file) t of
    Nothing -> Nothing
    Just f  -> do
      let fs = FS file { contents = sortStack . FileStack $
                           (getFileStack $ contents file) { stackCurrent = f } }
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
  case taskStart' time . stackCurrent . getFileStack $ contents file of
    Nothing    -> Nothing
    Just file' -> Just $
      FS (file { contents = FileStack $
                   (getFileStack $ contents file) { stackCurrent = file' } })
         prev

taskStart' :: UTCTime -> File -> Maybe File
taskStart' time task@Task{} = Just $ task { status = Just $ Tracking time }
taskStart' _ _ = Nothing

taskEnd :: UTCTime -> FS -> Maybe FS
taskEnd _ (FS Empty _)      = Nothing
taskEnd _ (FS Note{} _)     = Nothing
taskEnd time (FS file prev) =
  case taskEnd' time . stackCurrent . getFileStack $ contents file of
    Nothing    -> Nothing
    Just file' -> Just $
      FS (file { contents = FileStack $
                   (getFileStack $ contents file) { stackCurrent = file' } })
         prev

taskEnd' :: UTCTime -> File -> Maybe File
taskEnd' time task@Task{} = case status task of
  Just (Tracking t) -> Just $ task { status   = Nothing
                                   , sessions = Session t time : sessions task }
  _ -> Nothing
taskEnd' _ _ = Nothing

setPriority :: Int -> FS -> Maybe FS
setPriority _ (FS Empty _)   = Nothing
setPriority n (FS file prev) =
  case setPriority' n . stackCurrent . getFileStack $ contents file of
    Nothing    -> Nothing
    Just file' -> do
      let fs = FS (file { contents = sortStack . FileStack $
                            (getFileStack $ contents file) { stackCurrent =
                                                               file' } })
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
taskSetStatus f (FS file prev) =
  case f . stackCurrent . getFileStack $ contents file of
    Nothing    -> Nothing
    Just file' -> do
      let fs = FS (file { contents = sortStack . FileStack $
                            (getFileStack $ contents file) { stackCurrent =
                                                               file' } })
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
taskSetTODO' time file@Task{} = Just $ file { todoDate = Just time }
taskSetTODO' _ _ = Nothing

taskUnsetTODO :: FS -> Maybe FS
taskUnsetTODO = taskSetStatus taskUnsetTODO'

taskUnsetTODO' :: File -> Maybe File
taskUnsetTODO' file@Task{} = Just $ file { todoDate = Nothing }
taskUnsetTODO' _           = Nothing

taskUnsetStatus :: FS -> Maybe FS
taskUnsetStatus = taskSetStatus taskUnsetStatus'

taskUnsetStatus' :: File -> Maybe File
taskUnsetStatus' file@Task{} = Just $ file { status = Nothing }
taskUnsetStatus' _           = Nothing

stackDeleteCurrent :: Stack a -> Maybe (Stack a)
stackDeleteCurrent (Stack [] _ [])       = Nothing
stackDeleteCurrent (Stack [] _ (d : ds)) = Just $ Stack [] d ds
stackDeleteCurrent (Stack (u : us) _ []) = Just $ Stack us u []
stackDeleteCurrent (Stack u _ (d : ds))  = Just $ Stack u d ds

sortFile :: [File] -> [File]
sortFile [ Empty ] = [ Empty ]
sortFile x         =
  sortOn (T.toLower <$> (\file -> T.pack (show (priority file)) `T.append` " "
                         `T.append` name file))
         x

sortStack :: FileStack -> FileStack
sortStack (FileStack stack) = stackFromList . sortFile $ stackToList stack

stackToList :: Stack a -> [a]
stackToList (Stack u c d) = reverse u ++ c : d

stackFromList :: [File] -> FileStack
stackFromList []       = emptyStack
stackFromList (x : xs) = FileStack $ Stack [] x xs

fileSearch :: Text -> FS -> Maybe FS
fileSearch _ (FS Empty _)   = Nothing
fileSearch n (FS file prev) = case fileSearch' n file of
  Nothing    -> Nothing
  Just file' -> Just $ FS file' prev

fileSearch' :: Text -> File -> Maybe File
fileSearch' _ Empty = Nothing
fileSearch' _ Note{} = Nothing
fileSearch' n container =
  case stackCurrent . getFileStack $ contents container of
    Empty -> Nothing
    file  -> if T.toLower n `T.isInfixOf` T.toLower (name file)
             then Just container
             else lookNext container `mplus` lookPrev container
  where lookPrev       = look fileUp'

        lookNext       = look fileDown'

        look func nec_ = do
          nec' <- func nec_
          case stackCurrent . getFileStack $ contents nec' of
            Empty -> Nothing
            file' -> if T.toLower n `T.isInfixOf` T.toLower (name file')
                     then Just nec'
                     else look func nec'

emptyStack :: FileStack
emptyStack = FileStack $ Stack [] Empty []

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
                   , todoDate = Nothing
                   , priority = 0 }

getFSPath :: FS -> [Text]
getFSPath fs@(FS file _) =
  let n = case stackCurrent . getFileStack $ contents file of
        Empty       -> "Empty"
        note@Note{} -> name note
        file'       -> name file'
  in
      case fsOut fs of
        Nothing  -> [ n ]
        Just fs' -> getFSPath fs' ++ [ n ]

goToFSPath :: [Text] -> FS -> Maybe FS
goToFSPath path fs = fsTop fs >>= goToFSPath' path

goToFSPath' :: [Text] -> FS -> Maybe FS
goToFSPath' [] fs       = Just fs
goToFSPath' [ x ] fs    = fileSearch x fs
goToFSPath' (x : xs) fs = fileSearch x fs >>= fsIn >>= goToFSPath' xs

emptyFS :: Text -> FS
emptyFS n = FS (emptyFolder n) []

emptyNote :: Text -> UTCTime -> File
emptyNote n date = Note { name     = n
                        , noteDate = date
                        , priority = 0 }
