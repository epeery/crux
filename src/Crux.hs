{-# LANGUAGE OverloadedStrings #-}

module Crux where

import           Brick.Main           as B

import           Crux.Actions
import           Crux.Actions.Browser
import           Crux.Actions.Utils   ( fromMaybe )
import           Crux.App
import           Crux.Browser
import           Crux.Core
import           Crux.Data
import           Crux.Data.Types
import           Crux.FS

import qualified Data.Map.Strict      as M

import           System.Exit          ( die )

crux :: IO ()
crux = do
  ecf <- readCruxFile
  case ecf of
    Left s   -> do
      putStrLn "Save file is formatted incorrectly"
      fp <- getCruxFilePath
      putStrLn $ "File is located at: " ++ fp
      error s
    Right cf -> do
      lock <- lockCruxFile
      case lock of
        Nothing -> die "Failed to lock data file. Is Crux already running?"
        Just fl -> do
          s <- startCrux cf
          writeCruxFile (CruxFile (fsCurrent . fromMaybe fsTop $ browserCursor $
                                   cruxBrowserState s)
                                  (cruxTodos s))
          unlockCruxFile fl

startCrux :: CruxFile -> IO CruxState
startCrux cf = defaultMain (cruxApp defaultConfig) (defaultState cf)

defaultConfig :: CruxConfig
defaultConfig =
  CruxConfig { configBindings =
                 ViewBindings { browserNormalBindings =
                                  M.fromList [ (BChar '?', showHelp)
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
                                             , (BKey "Tab", showTODOList)
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
                                             , (BChar 't', toggleTaskTODO)
                                             , (BChar 'p', browserPriorityMode) ]
                              , browserGBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'g', browserGHigh) ]
                              , browserTODOBindings =
                                  M.fromList [ (BAll, browserNormalMode)
                                             , (BChar 'q', showProjectBrowser)
                                             , (BKey "Tab", showProjectBrowser)
                                             , ( BKey "Escape"
                                                 , showProjectBrowser
                                                 )
                                             , (BKey "Enter", todoOpen)
                                             , (BChar 'j', todoDown)
                                             , (BChar 'k', todoUp) ] } }

defaultState :: CruxFile -> CruxState
defaultState (CruxFile cf tl) =
  CruxState { cruxBrowserState = BrowserState (FS cf []) BrowserNormal
            , cruxActiveView = startingView cf
            , cruxTodos = tl
            , cruxFSPath = []
            , key = Nothing }

startingView :: File -> View
startingView Task{} = Browser
startingView f      = case contents f of
  Stack [] Empty [] -> Info
  _ -> Browser
