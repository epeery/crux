{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Crux.Data.Types ( CruxFile(..) ) where

import           Crux.FS

import           Data.Aeson
import qualified Data.Vector  as Vector

import           GHC.Generics ( Generic )

------------------------------------------------------------------------
--                             Datatypes                              --
------------------------------------------------------------------------
newtype CruxFile = CruxFile { cruxFileContents :: File }
  deriving ( Show, Eq, Generic, FromJSON, ToJSON )

------------------------------------------------------------------------
--                           JSON instances                           --
------------------------------------------------------------------------
instance FromJSON FileStack where
  parseJSON = withArray "FileStack" $ \arr -> stackFromList
    <$> mapM parseJSON (Vector.toList arr)

instance ToJSON FileStack where
  toJSON s = toJSON . stackToList $ getFileStack s

instance FromJSON File where
  parseJSON = genericParseJSON (idiomaticJsonOptions id)

instance ToJSON File where
  toJSON = genericToJSON (idiomaticJsonOptions id)

instance FromJSON TaskStatus where
  parseJSON = genericParseJSON (idiomaticJsonOptions id)

instance ToJSON TaskStatus where
  toJSON = genericToJSON (idiomaticJsonOptions id)

instance FromJSON Session where
  parseJSON = genericParseJSON (idiomaticJsonOptions (drop 7))

instance ToJSON Session where
  toJSON = genericToJSON (idiomaticJsonOptions (drop 7))

------------------------------------------------------------------------
--                             Functions                              --
------------------------------------------------------------------------
idiomaticJsonOptions :: (String -> String) -> Options
idiomaticJsonOptions f =
  defaultOptions { constructorTagModifier = camelTo2 '_' . filter (/= '\'')
                 , fieldLabelModifier = camelTo2 '_' . f
                 , sumEncoding = ObjectWithSingleField
                 , tagSingleConstructors = True }

