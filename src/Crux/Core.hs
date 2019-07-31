{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Crux.Core
  ( module Crux.Core
  , module Control.Monad.State
  , module Control.Monad.Reader
  ) where

import           Brick.Types          as B hiding ( Next )

import           Control.Monad.Reader
import           Control.Monad.State

import           Crux.Browser         ( BrowserState )
import           Crux.FS

import           Data.Map.Strict      ( Map )
import           Data.String          ( IsString )
import           Data.Text            ( Text )
import qualified Data.Text            as T ( unpack )

import           GHC.Generics         ( Generic )

newtype CruxConfig = CruxConfig { configBindings :: ViewBindings }

data ViewBindings =
  ViewBindings { browserNormalBindings :: Bindings
               , browserInsertBindings :: Bindings
               , browserFolderCreateBindings :: Bindings
               , browserProjectCreateBindings :: Bindings
               , browserTaskCreateBindings :: Bindings
               , browserDeleteBindings :: Bindings
               , browserPriorityBindings :: Bindings
               , browserTaskBindings :: Bindings
               , browserGBindings :: Bindings
               , browserTODOBindings :: Bindings }

type Bindings = Map Binding Action

data Binding = BChar Char
             | BKey Text
             | BAll
  deriving ( Eq, Ord )

newtype ActionName = ActionName { actionNameText :: Text }
  deriving ( Read, Eq, Ord, Generic, IsString, Semigroup, Monoid )

data Action = Action { actionName :: ActionName
                     , actionDesc :: Text
                     , actionFunc :: Crux () }

instance Show Action where
  show = show . actionName

instance Show ActionName where
  show = T.unpack . actionNameText

data CruxState = CruxState { cruxBrowserState :: BrowserState
                           , cruxTodos :: File
                           , cruxFSPath :: [Text]
                           , cruxActiveView :: View
                           , key :: Maybe Binding }
  deriving ( Eq, Generic )

data View = Browser
          | Help
          | TODOList
          | Info
  deriving ( Show, Eq, Generic )

newtype ResourceName = ResourceName Text
  deriving ( Show, Eq, Ord, Generic, IsString )

newtype Crux a =
  Crux { unCrux :: NextT (StateT CruxState (ReaderT CruxConfig (EventM ResourceName))) a }
  deriving ( Generic, Functor, Applicative, Monad, MonadState CruxState
           , MonadReader CruxConfig )

instance MonadIO Crux where
  liftIO = Crux . liftIO

runCrux :: CruxConfig
        -> CruxState
        -> Crux a
        -> EventM ResourceName (MStop a, CruxState)
runCrux conf initState act =
  runReaderT (runStateT (runNextT (unCrux act)) initState) conf

data CruxEvent = CruxSaveFile

data MStop a = Stop
             | Continue a
  deriving ( Show, Eq, Generic )

instance Functor MStop where
  fmap _ Stop         = Stop
  fmap f (Continue a) = Continue $ f a

newtype NextT m a = NextT { runNextT :: m (MStop a) }

instance Functor m => Functor (NextT m) where
  fmap f (NextT func) = NextT $ fmap (f <$>) func

instance Monad m => Applicative (NextT m) where
  pure = NextT . pure . Continue

  (NextT f1) <*> (NextT f2) = NextT $ do
    n1 <- f1
    case n1 of
      Stop       -> pure Stop
      Continue f -> do
        n2 <- f2
        pure $ f <$> n2

instance Monad m => Monad (NextT m) where
  (NextT ma) >>= fm = NextT $ do
    na <- ma
    case na of
      Stop       -> pure Stop
      Continue a -> runNextT $ fm a

instance MonadTrans NextT where
  lift func = NextT $ Continue <$> func

instance MonadIO m => MonadIO (NextT m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (NextT m) where
  get = NextT $ gets Continue

  put = NextT . fmap Continue . put

instance MonadReader s m => MonadReader s (NextT m) where
  ask = NextT $ asks Continue

  local func (NextT m) = NextT $ local func m
