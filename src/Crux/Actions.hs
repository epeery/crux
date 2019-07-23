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
