{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.OptParse.Types where

import Import

import Text.PrettyPrint.ANSI.Leijen (Doc)

import Smos.Data

data Arguments =
    Arguments String
              Flags

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Instructions =
    Instructions ([(Path Abs File, SmosFile)] -> Doc)
                 Settings

data Settings =
    Settings
