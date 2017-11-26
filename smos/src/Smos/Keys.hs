module Smos.Keys
    ( Keymap
    , matchChar
    , satisfyChar
    , onChar
    , onCharM
    , afterChar
    , matchKey
    , satisfyKey
    , onKey
    , onKeyM
    , afterKey
    , matchKeyPress
    , satisfyKeyPress
    , onKeyPress
    , onKeyPressM
    , matchEvent
    , satisfyEvent
    , onEvent
    , onEventM
    -- * Filters
    , inEmpty
    , inEntry
    , inHeader
    , inContents
    , inTags
    -- * Raw building blocks
    , filterKeymap
    , rawKeymap
    -- * Re-exports
    , KeyPress(..)
    , V.Key(..)
    , V.Modifier(..)
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
            B.VtyEvent (V.EvKey (V.KChar ec) []) ->
                if pred_ ec
                    then Just func
                    else Nothing
            _ -> Nothing

onChar :: (Char -> SmosM ()) -> Keymap
onChar func = onCharM (Just . func)

onCharM :: (Char -> Maybe (SmosM ())) -> Keymap
onCharM func =
    onKeyM $ \k ->
        case k of
            V.KChar c -> func c
            _ -> Nothing

afterChar :: Char -> Keymap -> Keymap
afterChar = afterKey . V.KChar

matchKey :: V.Key -> SmosM () -> Keymap
matchKey k = satisfyKey (== k)

satisfyKey :: (V.Key -> Bool) -> SmosM () -> Keymap
satisfyKey pred_ =
    satisfyKeyPress $ \kp ->
        case kp of
            KeyPress k [] -> pred_ k
            _ -> False

onKey :: (V.Key -> SmosM ()) -> Keymap
onKey func = onKeyM (Just . func)

onKeyM :: (V.Key -> Maybe (SmosM ())) -> Keymap
onKeyM func =
    onKeyPressM $ \kp ->
        case kp of
            KeyPress k [] -> func k
            _ -> Nothing

afterKey :: V.Key -> Keymap -> Keymap
afterKey k = afterKeypress $ KeyPress k []

matchKeyPress :: KeyPress -> SmosM () -> Keymap
matchKeyPress kp = satisfyKeyPress (== kp)

satisfyKeyPress :: (KeyPress -> Bool) -> SmosM () -> Keymap
satisfyKeyPress pred_ =
    satisfyEvent $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey ek mods) ->
                let kp = KeyPress ek mods
                in pred_ kp
            _ -> False

onKeyPress :: (KeyPress -> SmosM ()) -> Keymap
onKeyPress func = onKeyPressM (Just . func)

onKeyPressM :: (KeyPress -> Maybe (SmosM ())) -> Keymap
onKeyPressM func =
    onEventM $ \ev ->
        case ev of
            B.VtyEvent (V.EvKey ek mods) ->
                let kp = KeyPress ek mods
                in func kp
            _ -> Nothing

matchEvent :: B.BrickEvent ResourceName () -> SmosM () -> Keymap
matchEvent e = satisfyEvent (== e)

satisfyEvent :: (B.BrickEvent ResourceName () -> Bool) -> SmosM () -> Keymap
satisfyEvent pred_ func =
    onEventM $ \ev ->
        if pred_ ev
            then Just func
            else Nothing

onEvent :: (B.BrickEvent ResourceName () -> SmosM ()) -> Keymap
onEvent func = onEventM (Just . func)

onEventM :: (B.BrickEvent ResourceName () -> Maybe (SmosM ())) -> Keymap
onEventM = rawKeymap

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

inTags :: Keymap -> Keymap
inTags =
    filterKeymap $ \s ->
        case smosStateCursor s of
            Just (ATags _) -> True
            _ -> False
