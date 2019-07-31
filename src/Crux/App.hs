{-# LANGUAGE RecordWildCards #-}

module Crux.App where

import           Brick.Main         as B
import           Brick.Types        as B

import           Crux.Actions.Utils ( modifyActiveView )
import           Crux.Bindings      ( eventToBinding )
import           Crux.Browser
import           Crux.Core
import           Crux.Draw
import           Crux.Style

import qualified Data.Map.Strict    as M

import qualified Graphics.Vty       as Vty

type Event = BrickEvent ResourceName CruxEvent

cruxApp :: CruxConfig -> App CruxState CruxEvent ResourceName
cruxApp conf@CruxConfig{..} =
  App { appDraw         = cruxDraw conf
      , appChooseCursor = cruxChooseCursor
      , appHandleEvent  = cruxHandleEvent conf
      , appStartEvent   = pure
      , appAttrMap      = defaultAttrMap }

cruxChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
cruxChooseCursor = showFirstCursor

cruxHandleEvent
  :: CruxConfig -> CruxState -> Event -> EventM ResourceName (Next CruxState)
cruxHandleEvent conf st e = case e of
  VtyEvent vtye -> do
    let act = handleKey conf st vtye
    (nextEvent, st') <- runCrux conf st act
    case nextEvent of
      Stop        -> halt st'
      Continue () -> continue st'
  _ -> B.continue st

handleKey :: CruxConfig -> CruxState -> Vty.Event -> Crux ()
handleKey CruxConfig{..} s@CruxState{..} e = case eventToBinding e of
  Nothing -> pure ()
  Just a  -> do
    put (s { key = Just a })
    case cruxActiveView of
      Browser  -> case browserMode of
        BrowserNormal    -> getAction browserNormalBindings a
        BrowserInsert{}  -> getAction browserInsertBindings a
        BrowserCommand c -> case c of
          BCFolderCreate -> getAction browserFolderCreateBindings a
          BCProjectCreate -> getAction browserProjectCreateBindings a
          BCPriority -> getAction browserPriorityBindings a
          BCTaskCreate -> getAction browserTaskCreateBindings a
          BCDelete -> getAction browserDeleteBindings a
          BCTask -> getAction browserTaskBindings a
          BCG -> getAction browserGBindings a
      Help     -> modifyActiveView Browser
      TODOList -> getAction browserTODOBindings a
      Info     -> modifyActiveView Browser *> getAction browserNormalBindings a
  where ViewBindings{..} = configBindings

        BrowserState{..} = cruxBrowserState

getAction :: Bindings -> Binding -> Crux ()
getAction binds e = case validBinds e of
  Nothing -> case validBinds BAll of
    Nothing -> pure ()
    Just a  -> actionFunc a
  Just a  -> actionFunc a
  where validBinds b = M.lookup b binds
