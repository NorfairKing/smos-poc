{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smos.Convert.OptParse.Types where

import Import

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype ConvertFileArgs = ConvertFileArgs
    { convertArgsFiles :: [FilePath]
    } deriving (Show, Eq)

newtype Command =
    ConvertFile ConvertFileArgs
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)

newtype Flags = Flags
    { flgTodoStates :: [Text]
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data DispatchConvertFileArgs = DispatchConvertFileArgs
    { dispatchConvertPaths :: [Path Abs File]
    , dispatchTodoStates :: [Text]
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchConvertFile DispatchConvertFileArgs
    deriving (Show, Eq)
