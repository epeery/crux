{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Draw where

import           Brick.Types          as B
import           Brick.Widgets.Center as B
import           Brick.Widgets.Core   as B

import           Crux.Core
import           Crux.Draw.Browser    ( renderBrowser )
import           Crux.Draw.Todo       ( renderTodo )
import           Crux.Style

cruxDraw :: CruxConfig -> CruxState -> [Widget ResourceName]
cruxDraw conf st = case cruxActiveView of
  Info     -> [ drawInfo ]
  Help     -> [ txt "TODO: Actually make a help menu" ]
  Browser  -> [ renderBrowser conf cruxBrowserState ]
  TODOList -> [ renderTodo cruxTodos ]
  where CruxState{..} = st

drawInfo :: Widget n
drawInfo = withAttr selectedAttr $ vCenterLayer $ vBox $
  map B.hCenterLayer
      [ str "CRUX v0.1.0.0"
      , str " "
      , hBox [ str "Press "
             , withAttr folder (str "nf")
             , withAttr entry $ str " to create a new folder" ]
      , hBox [ str "Press "
             , withAttr folder (str "ne")
             , withAttr entry $ str " to to create a new entry" ]
      , hBox [ str "Press "
             , withAttr folder (str "q")
             , withAttr entry $ str " to quit the program" ]
      , str " "
      , hBox [ str "Press "
             , withAttr folder (str "?")
             , withAttr entry $ str " for a full list of key bindings" ]
      , str " "
      , str "Made by Eli Peery"
      , str "https://epeery.com" ]
