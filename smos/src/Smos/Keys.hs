module Smos.Keys
    ( Keymap
    , matchChar
    , satisfyChar
    , matchKey
    , onChar
    , satisfyKey
    , rawKeymap
    , V.Key(..)
    , Monoid(..)
    ) where

import Import

import qualified Brick.Types as B
import qualified Graphics.Vty as V

import Smos.Types

matchChar :: Char -> SmosM () -> Keymap e
matchChar c = matchKey $ V.KChar c

satisfyChar :: (Char -> Bool) -> SmosM () -> Keymap e
satisfyChar pred_ func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey (V.KChar ec) []) -> when (pred_ ec) func
            _ -> pure ()

matchKey :: V.Key -> SmosM () -> Keymap e
matchKey k = satisfyKey (== k)

onChar :: (Char -> SmosM ()) -> Keymap e
onChar func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey (V.KChar c) []) -> func c
            _ -> pure ()

satisfyKey :: (V.Key -> Bool) -> SmosM () -> Keymap e
satisfyKey pred_ func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey ek []) -> when (pred_ ek) func
            _ -> pure ()
