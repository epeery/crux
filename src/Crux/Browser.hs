module Crux.Browser where

import           Crux.FS

import           Data.Text ( Text )

data BrowserState = BrowserState { browserCursor :: FS
                                 , browserMode   :: BrowserMode }
  deriving ( Show, Eq )

data BrowserMode = BrowserNormal
                 | BrowserInsert BInsert Text
                 | BrowserCommand BCommand
  deriving ( Show, Eq )

data BInsert = BIFolder
             | BIProject
             | BITask
             | BINote
             | BIRename
             | BISearch
  deriving ( Show, Eq )

data BCommand = BCTask
              | BCFolderCreate
              | BCProjectCreate
              | BCTaskCreate
              | BCDelete
              | BCPriority
              | BCG
  deriving ( Show, Eq )
