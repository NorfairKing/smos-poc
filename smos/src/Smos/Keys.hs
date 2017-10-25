module Smos.Keys
    ( Keymap
    , matchChar
    , matchKey
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

matchKey :: V.Key -> SmosM () -> Keymap e
matchKey k func =
    rawKeymap $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey ek []) -> when (k == ek) func
            _ -> pure ()
