module Main where

import           Crux
import           Crux.Actions.Utils ( fromMaybe )
import           Crux.Browser
import           Crux.Core
import           Crux.Data
import           Crux.Data.Types
import           Crux.FS

main :: IO ()
main = do
  s <- crux
  writeCruxFile (CruxFile (fsCurrent . fromMaybe fsTop $ browserCursor $
                           cruxBrowserState s)
                          (cruxTodos s)
                          (cruxPreviousTodos s))
