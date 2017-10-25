{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types where

import Import

import Data.Map (Map)
import Data.String

import Control.Monad.State

import Brick.Types as B hiding (Next)

import Smos.Cursor

-- TODO remove these once they're in the library.
deriving instance Ord Location

deriving instance (Ord n, Ord e) => Ord (BrickEvent n e)

newtype SmosConfig e = SmosConfig
    { keyMap :: Map (BrickEvent ResourceName e) (SmosM ())
    } deriving (Generic)

type SmosM = MkSmosM ResourceName SmosState

runSmosM :: SmosState -> SmosM a -> EventM ResourceName (MStop a, SmosState)
runSmosM = runMkSmosM

newtype SmosState = SmosState
    { smosStateCursor :: Maybe ACursor
    } deriving (Generic)

newtype ResourceName = ResourceName
    { unResourceName :: Text
    } deriving (Show, Eq, Ord, Generic, IsString)

newtype MkSmosM n s a = MkSmosM
    { unMkSmosM :: NextT (StateT s (EventM n)) a
    } deriving (Generic, Functor, Applicative, Monad)

instance MonadState s (MkSmosM n s) where
    get = MkSmosM $ lift get
    put = MkSmosM . lift . put

runMkSmosM :: s -> MkSmosM n s a -> EventM n (MStop a, s)
runMkSmosM initState act = runStateT (runNextT (unMkSmosM act)) initState

data MStop a
    = Stop
    | Continue a

instance Functor MStop where
    fmap _ Stop = Stop
    fmap f (Continue a) = Continue $ f a

newtype NextT m a = NextT
    { runNextT :: m (MStop a)
    }

instance Functor m => Functor (NextT m) where
    fmap f (NextT func) = NextT $ fmap (f <$>) func

instance Monad m => Applicative (NextT m) where
    pure = NextT . pure . Continue
    (NextT f1) <*> (NextT f2) =
        NextT $ do
            n1 <- f1
            case n1 of
                Stop -> pure Stop
                Continue f -> do
                    n2 <- f2
                    pure $ f <$> n2

instance Monad m => Monad (NextT m) where
    (NextT ma) >>= fm =
        NextT $ do
            na <- ma
            case na of
                Stop -> pure Stop
                Continue a -> runNextT $ fm a

instance MonadTrans NextT where
    lift func = NextT $ Continue <$> func
