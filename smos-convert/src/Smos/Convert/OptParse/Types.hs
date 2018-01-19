{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype ConvertFileArgs = ConvertFileArgs
    { orgfile :: FilePath
    } deriving (Show, Eq)

newtype Command =
    ConvertFile ConvertFileArgs
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype DispatchConvertFileArgs = DispatchConvertFileArgs
    { orgpath :: Path Abs File
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchConvertFile DispatchConvertFileArgs
    deriving (Show, Eq)
