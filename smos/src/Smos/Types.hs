{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Types where

import Import

import Data.Map (Map)
import Data.String

import Control.Monad.State

import Brick.Types as B

import Smos.Cursor

newtype SmosConfig e = SmosConfig
    { keyMap :: Map (BrickEvent ResourceName e) (SmosM ())
    } deriving (Generic)

data MkNext f a
    = MkHalt -- ^ Stop here
    | MkPure a -- ^ A pure value
    | MkCont (f (MkNext f a)) -- ^ Do something, and then continue
    | MkSusp (IO (f (MkNext f a))) -- ^ Suspend the brick app, do some IO, produce a next action to go on from.

instance Functor f => Functor (MkNext f) where
    fmap _ MkHalt = MkHalt
    fmap f (MkPure a) = MkPure $ f a
    fmap f (MkCont a) = MkCont $ fmap f <$> a
    fmap f (MkSusp a) =
        MkSusp $ do
            mkn <- a
            pure $ fmap f <$> mkn

instance Applicative f => Applicative (MkNext f) where
    pure = MkPure
    MkHalt <*> _ = MkHalt
    MkPure fa <*> mka = fa <$> mka
    -- f (MkNext f (a -> b))
    -- MkNext f b
    MkCont ffa <*> mka = MkCont $ fmap (<*> mka) ffa
    MkSusp ionfa <*> mka =
        MkSusp $ do
            mna <- ionfa
            pure $ fmap (<*> mka) mna

instance Applicative f => Monad (MkNext f) where
    MkHalt >>= _ = MkHalt
    MkPure a >>= fmkb = fmkb a
    MkCont fa >>= fmkb = MkCont $ fmap (>>= fmkb) fa
    MkSusp iomka >>= fmkb =
        MkSusp $ do
            mka <- iomka
            pure $ fmap (>>= fmkb) mka

newtype SmosState = SmosState
    { smosStateCursor :: ACursor
    } deriving (Generic)

newtype ResourceName = ResourceName
    { unResourceName :: Text
    } deriving (Show, Eq, Ord, Generic, IsString)

newtype SmosM a = SmosM
    { runSmosM :: StateT SmosState (MkNext (EventM ResourceName)) a
    } deriving (Generic, Functor, Applicative, Monad, MonadState SmosState)

halt :: SmosM a
halt = SmosM $ lift MkHalt

suspendAndResume :: IO a -> SmosM a
suspendAndResume func = SmosM $ lift $ MkSusp $ func >>= (pure . pure . pure)

liftEventM :: EventM ResourceName a -> SmosM a
liftEventM func = SmosM $ lift $ MkCont $ func >>= (pure . pure)
