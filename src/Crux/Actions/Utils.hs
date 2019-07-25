{-# LANGUAGE RecordWildCards #-}

module Crux.Actions.Utils where

import           Crux.Browser
import           Crux.Core
import           Crux.FS

import           Data.Text    ( Text )

modifyBrowserCursor :: (FS -> Maybe FS) -> Crux ()
modifyBrowserCursor f = do
  s <- get
  let CruxState{..}    = s
      BrowserState{..} = cruxBrowserState
  put (s { cruxBrowserState =
             cruxBrowserState { browserCursor = fromMaybe f browserCursor } })

modifyBrowserInsert :: BInsert -> Text -> Crux ()
modifyBrowserInsert m t = modifyBrowserMode (BrowserInsert m t)

modifyActiveView :: View -> Crux ()
modifyActiveView v = modify (\cs -> cs { cruxActiveView = v })

modifyBrowserMode :: BrowserMode -> Crux ()
modifyBrowserMode m =
  modify (\cs ->
          cs { cruxBrowserState = (cruxBrowserState cs) { browserMode = m } })

fromMaybe :: (a -> Maybe a) -> a -> a
fromMaybe f x = case f x of
  Nothing -> x
  Just a  -> a

modifyPriority :: Int -> Crux ()
modifyPriority n = modifyBrowserCursor (setPriority (n * (-1)))
  *> modifyBrowserMode BrowserNormal
