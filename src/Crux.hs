{-# LANGUAGE OverloadedStrings #-}

module Crux where

import           Brick.Main           as B

import           Crux.Actions
import           Crux.Actions.Browser
import           Crux.App
import           Crux.Browser
import           Crux.Core
import           Crux.Data
import           Crux.Data.Types
import           Crux.FS

import qualified Data.Map.Strict      as M

crux = do
  ecf <- readCruxFile
  case ecf of
    Left s   -> error "Save file is formatted incorrectly"
    Right cf -> startCrux cf

startCrux :: CruxFile -> IO CruxState
startCrux cf = defaultMain (cruxApp defaultConfig) (defaultState cf)

defaultConfig =
  CruxConfig { configBindings =
                 ViewBindings { browserNormalBindings =
                                  M.fromList [ (BChar '?', showHelp)
                                             , (BKey "Escape", quit)
                                             , (BChar 'q', quit)
                                             , (BChar 'j', browserDown)
                                             , (BChar 'k', browserUp)
                                             , (BChar 'l', browserOpen)
                                             , (BChar 'h', browserBack)
                                             , (BKey "Down", browserDown)
                                             , (BKey "Up", browserUp)
                                             , (BKey "Right", browserOpen)
                                             , (BKey "Left", browserBack)
                                             , (BChar 'T', browserTop)
                                             , (BChar 'H', browserHigh)
                                             , (BChar 'L', browserLow)
                                             , (BChar 'G', browserLow)
                                             , (BChar 'g', browserGMode)
                                             , (BChar 'n', browserCreateMode)
                                             , (BChar 'd', browserDeleteMode)
                                             , (BChar 'r', browserRename)
                                             , (BChar '/', browserSearch)
                                             , (BKey "Space", browserTaskMode) ]
                              , browserFolderCreateBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'f', newFolder)
                                             , (BChar 'e', newEntry) ]
                              , browserEntryCreateBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'e', newTask) ]
                              , browserDeleteBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'D', browserDelete) ]
                              , browserPriorityBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar '0', priority0)
                                             , (BChar '1', priority1)
                                             , (BChar '2', priority2)
                                             , (BChar '3', priority3)
                                             , (BChar '4', priority4)
                                             , (BChar '5', priority5)
                                             , (BChar '6', priority6)
                                             , (BChar '7', priority7)
                                             , (BChar '8', priority8)
                                             , (BChar '9', priority9) ]
                              , browserTaskCreateBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'e', newNote) ]
                              , browserInsertBindings =
                                  M.fromList [ (BAll, browserInsertAppend)
                                             , ( BKey "Escape"
                                                 , browserNormalMode
                                                 ) ]
                              , browserTaskBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , ( BKey "Space"
                                                 , browserTrackToggle
                                                 )
                                             , (BKey "Enter", toggleTaskDone)
                                             , (BChar 'p', browserPriorityMode) ]
                              , browserGBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'g', browserGHigh) ] } }

defaultState (CruxFile cf) =
  CruxState { cruxBrowserState = BrowserState (FS cf []) BrowserNormal
            , cruxActiveView = startingView cf
            , key = Nothing }

startingView :: File -> View
startingView Task{} = Browser
startingView f      = case contents f of
  Stack [] Empty [] -> Info
  _ -> Browser
