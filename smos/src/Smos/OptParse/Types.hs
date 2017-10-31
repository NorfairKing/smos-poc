{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import Smos.Types

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandEdit FilePath
    | CommandReport String
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Settings =
    Settings

data Dispatch
    = DispatchEdit (Path Abs File)
    | DispatchReport Report
