{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types
    ( SmosConfig(..)
    , Keymap(..)
    , afterKeypress
    , filterKeymap
    , rawKeymap
    , SmosEvent
    , SmosM
    , runSmosM
    , SmosState(..)
    , KeyPress(..)
    , ResourceName
    , MStop(..)
    , stop
    , module Control.Monad.Reader
    , module Control.Monad.State
    ) where

import Import

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Vty.Input.Events

import Brick.AttrMap as B
import Brick.Types as B hiding (Next)

import Smos.Cursor

data SmosConfig = SmosConfig
    { configKeyMap :: Keymap
    , configAttrMap :: SmosState -> B.AttrMap
    , configAgendaFiles :: IO [Path Abs File]
    } deriving (Generic)

newtype Keymap = Keymap
    { unKeymap :: SmosState -> SmosEvent -> Maybe (SmosM ())
    } deriving (Generic)

afterKeypress :: KeyPress -> Keymap -> Keymap
afterKeypress kp (Keymap km) =
    Keymap $ \s e ->
        case reverse $ smosStateKeyHistory s of
            (kp':_) ->
                if kp == kp'
                    then km (s
                             { smosStateKeyHistory =
                                   drop 1 $ smosStateKeyHistory s
                             })
                             e
                    else Nothing
            _ -> Nothing

-- TODO explain how this is not the current state, but the state at the start of
-- the handler
filterKeymap :: (SmosState -> Bool) -> Keymap -> Keymap
filterKeymap pred_ (Keymap km) =
    Keymap $ \s e ->
        if pred_ s
            then km s e
            else Nothing

rawKeymap :: (SmosEvent -> Maybe (SmosM ())) -> Keymap
rawKeymap func =
    Keymap $ \s e ->
        case func e of
            Nothing -> Nothing
            Just f ->
                if null (smosStateKeyHistory s)
                    then Just f
                    else Nothing

-- | This instance is the most important for implementors.
--
-- It means you can 'combine keymaps'.
--
-- Note that all handlers will be executed, not just the first one to be
-- selected.
instance Monoid Keymap where
    mempty = Keymap $ \_ _ -> Nothing
    mappend (Keymap km1) (Keymap km2) =
        Keymap $ \s e ->
            case (km1 s e, km2 s e) of
                (Nothing, Nothing) -> Nothing
                (Just f1, Nothing) -> Just f1
                (Nothing, Just f2) -> Just f2
                (Just f1, Just f2) -> Just $ f1 >> f2

type SmosEvent = BrickEvent ResourceName ()

type SmosM = MkSmosM SmosConfig ResourceName SmosState

runSmosM ::
       SmosConfig
    -> SmosState
    -> SmosM a
    -> EventM ResourceName (MStop a, SmosState)
runSmosM = runMkSmosM

data SmosState = SmosState
    { smosStateFilePath :: Path Abs File
    , smosStateCursor :: Maybe ACursor
    , smosStateKeyHistory :: [KeyPress]
    } deriving (Generic)

data KeyPress =
    KeyPress Key
             [Modifier]
    deriving (Show, Eq, Ord)

newtype ResourceName =
    ResourceName Text
    deriving (Show, Eq, Ord, Generic, IsString)

newtype MkSmosM c n s a = MkSmosM
    { unMkSmosM :: NextT (StateT s (ReaderT c (EventM n))) a
    } deriving ( Generic
               , Functor
               , Applicative
               , Monad
               , MonadState s
               , MonadReader c
               )

instance MonadIO (MkSmosM c n s) where
    liftIO = MkSmosM . liftIO

runMkSmosM :: c -> s -> MkSmosM c n s a -> EventM n (MStop a, s)
runMkSmosM conf initState act =
    runReaderT (runStateT (runNextT (unMkSmosM act)) initState) conf

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

instance MonadIO m => MonadIO (NextT m) where
    liftIO = lift . liftIO

instance MonadState s m => MonadState s (NextT m) where
    get = NextT $ Continue <$> get
    put = NextT . fmap Continue . put

instance MonadReader s m => MonadReader s (NextT m) where
    ask = NextT $ Continue <$> ask
    local func (NextT m) = NextT $ local func m

stop :: SmosM a
stop = MkSmosM $ NextT $ pure Stop
