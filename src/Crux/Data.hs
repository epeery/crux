{-# LANGUAGE OverloadedStrings #-}

module Crux.Data
  ( emptyCruxFile
  , encodeCruxFile
  , getCruxFilePath
  , parseCruxFile
  , readCruxFile
  , writeCruxFile
  ) where

import           Crux.Data.Types
import           Crux.FS

import           Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty as JSON
import           Data.ByteString          ( ByteString )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL

import           Path.IO                  ( forgivingAbsence )

import           System.Directory         as D
import           System.FilePath          ( (</>) )

getCruxFilePath :: IO FilePath
getCruxFilePath = do
  xdg <- D.getXdgDirectory D.XdgConfig "crux"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "crux-data.json")

readCruxFile :: IO (Either String CruxFile)
readCruxFile = do
  p <- getCruxFilePath
  mContents <- forgivingAbsence $ BS.readFile p
  case mContents of
    Nothing       -> pure $ Right emptyCruxFile
    Just ""       -> pure $ Right emptyCruxFile
    Just contents -> pure $ parseCruxFile contents

parseCruxFile :: FromJSON a => ByteString -> Either String a
parseCruxFile = eitherDecode . BL.fromStrict

emptyCruxFile = CruxFile (emptyFolder "crux")

writeCruxFile :: CruxFile -> IO ()
writeCruxFile cf = do
  p <- getCruxFilePath
  BL.writeFile p (encodeCruxFile cf)

encodeCruxFile :: CruxFile -> BL.ByteString
encodeCruxFile = encodePretty
