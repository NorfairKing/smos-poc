module Smos.Default where

import Import

import qualified Data.Map as M

import Smos

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig e
defaultConfig = SmosConfig {keyMap = M.empty}
