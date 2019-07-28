{-# LANGUAGE OverloadedStrings #-}

module Crux.Actions where

import           Crux.Actions.Utils
import           Crux.Core

quit :: Action
quit = Action { actionName = "Quit"
              , actionDesc = "Exit the app"
              , actionFunc = Crux $ NextT $ pure Stop }

showHelp :: Action
showHelp = Action { actionName = "Help"
                  , actionDesc = "Show the help menu"
                  , actionFunc = modifyActiveView Help }

showTODOList :: Action
showTODOList = Action { actionName = "TODOs view"
                      , actionDesc = "Show a list of active TODO items"
                      , actionFunc = modifyActiveView TODOList }

showProjectBrowser :: Action
showProjectBrowser = Action { actionName = "Project browser view"
                            , actionDesc = "Show the project browser"
                            , actionFunc = modifyActiveView Browser }
