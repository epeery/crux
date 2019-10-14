{-# LANGUAGE OverloadedStrings #-}

module Crux.Actions where

import           Crux.Actions.Utils hiding ( fromMaybe )
import           Crux.Browser
import           Crux.Core
import           Crux.FS

import           Data.Maybe (fromMaybe)

quit :: Action
quit = Action { actionName = "Quit"
              , actionDesc = "Exit the app"
              , actionFunc = Crux $ NextT $ pure Stop }

showHelp :: Action
showHelp = Action { actionName = "Help"
                  , actionDesc = "Show the help menu"
                  , actionFunc = modifyActiveView Help }

showTODOList :: Action
showTODOList =
  Action { actionName = "TODOs view"
         , actionDesc = "Show a list of active TODO items"
         , actionFunc = do
             fs <- browserCursor . cruxBrowserState <$> get
             modify (\st -> st { cruxFSPath = getFSPath fs })
             modify (\st ->
                     st { cruxTodos =
                            foldr (\x acc -> case x of
                                     Task{} -> case path x of
                                       Nothing    -> acc
                                       Just path' -> case goToFSPath path' fs of
                                         Nothing         -> acc
                                         Just (FS dir _) ->
                                           fromMaybe acc (insertFile ((stackCurrent $ contents dir) { path = Just path' }) acc)
                                     _      -> acc)
                                  (emptyFolder "todo")
                                  (stackToList . contents $ cruxTodos st) })
             modifyTodoCursor fileTop'
             modifyActiveView TODOList }

showProjectBrowser :: Action
showProjectBrowser = Action { actionName = "Project browser view"
                            , actionDesc = "Show the project browser"
                            , actionFunc = modifyActiveView Browser }
