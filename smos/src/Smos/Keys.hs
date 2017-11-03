module Smos.Keys
    ( Keymap
    , matchChar
    , satisfyChar
    , matchKey
    , onChar
    , satisfyKey
    -- * Filters
    , inEmpty
    , inEntry
    , inHeader
    , inContents
    , inTodoState
    , inTag
    -- * Raw building blocks
    , filterKeymap
    , rawKeymap
    -- * Re-exports
    , V.Key(..)
    , Monoid(..)
    ) where

import Import

import qualified Brick.Types as B
import qualified Graphics.Vty as V

import Smos.Cursor
import Smos.Types

matchChar :: Char -> SmosM () -> Keymap
matchChar c = matchKey $ V.KChar c

satisfyChar :: (Char -> Bool) -> SmosM () -> Keymap
satisfyChar pred_ func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey (V.KChar ec) []) -> when (pred_ ec) func
            _ -> pure ()

matchKey :: V.Key -> SmosM () -> Keymap
matchKey k = satisfyKey (== k)

onChar :: (Char -> SmosM ()) -> Keymap
onChar func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey (V.KChar c) []) -> func c
            _ -> pure ()

satisfyKey :: (V.Key -> Bool) -> SmosM () -> Keymap
satisfyKey pred_ func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey ek []) -> when (pred_ ek) func
            _ -> pure ()

inEmpty :: Keymap -> Keymap
inEmpty =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Nothing -> True
            _ -> False

inEntry :: Keymap -> Keymap
inEntry =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (AnEntry _) -> True
            _ -> False

inHeader :: Keymap -> Keymap
inHeader =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (AHeader _) -> True
            _ -> False

inContents :: Keymap -> Keymap
inContents =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (AContents _) -> True
            _ -> False

inTodoState :: Keymap -> Keymap
inTodoState =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (AState _) -> True
            _ -> False

inTag :: Keymap -> Keymap
inTag =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (ATag _) -> True
            _ -> False
