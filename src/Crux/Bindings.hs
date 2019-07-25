{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crux.Bindings where

import           Crux.Core                 ( Binding(..) )

import qualified Data.Text                 as T

import           Graphics.Vty.Input.Events ( Event(..), Key(..) )

eventToBinding :: Event -> Maybe Binding
eventToBinding (EvKey (KChar ' ') []) = Just $ BKey "Space"
eventToBinding (EvKey (KChar '\^I') []) = Just $ BKey "Tab"
eventToBinding (EvKey (KChar c) []) = Just $ BChar c
eventToBinding (EvKey KEnter []) = Just $ BKey "Enter"
eventToBinding (EvKey KEsc []) = Just $ BKey "Escape"
eventToBinding (EvKey KBS []) = Just $ BKey "Backspace"
eventToBinding (EvKey KBackTab []) = Just $ BKey "Tab"
eventToBinding (EvKey KUp []) = Just $ BKey "Up"
eventToBinding (EvKey KLeft []) = Just $ BKey "Left"
eventToBinding (EvKey KDown []) = Just $ BKey "Down"
eventToBinding (EvKey KRight []) = Just $ BKey "Right"
eventToBinding _ = Nothing

instance Show Binding where
  show (BChar c)      = [ c ]
  show (BKey "Up")    = "↑"
  show (BKey "Left")  = "←"
  show (BKey "Down")  = "↓"
  show (BKey "Right") = "→"
  show (BKey k)       = T.unpack k
  show BAll           = "All"
