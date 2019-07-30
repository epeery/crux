{-# LANGUAGE OverloadedStrings #-}

module Crux.Data
  ( emptyCruxFile
  , encodeCruxFile
  , getCruxFilePath
  , lockCruxFile
  , parseCruxFile
  , readCruxFile
  , unlockCruxFile
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
import qualified System.FileLock          as FL
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
    Nothing      -> pure $ Right emptyCruxFile
    Just ""      -> pure $ Right emptyCruxFile
    Just content -> pure $ parseCruxFile content

lockCruxFile :: IO (Maybe FL.FileLock)
lockCruxFile = do
  p <- getCruxFilePath
  FL.tryLockFile p FL.Exclusive

unlockCruxFile :: FL.FileLock -> IO ()
unlockCruxFile = FL.unlockFile

parseCruxFile :: FromJSON a => ByteString -> Either String a
parseCruxFile = eitherDecode . BL.fromStrict

emptyCruxFile :: CruxFile
emptyCruxFile = CruxFile (emptyFolder "crux") (emptyFolder "todo")

writeCruxFile :: CruxFile -> IO ()
writeCruxFile cf = do
  p <- getCruxFilePath
  BL.writeFile p (encodeCruxFile cf)

encodeCruxFile :: CruxFile -> BL.ByteString
encodeCruxFile = encodePretty
