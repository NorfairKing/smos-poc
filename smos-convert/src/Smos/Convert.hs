{-# LANGUAGE RecordWildCards #-}

module Smos.Convert
    ( smosConvert
    ) where

import Import

import Smos.Convert.Document
import Smos.Convert.OptParse
import Smos.Convert.SmosFile

import Smos.Data

import qualified Data.Text.IO as T

smosConvert :: IO ()
smosConvert = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchConvertFile DispatchConvertFileArgs {..}) _ =
    mapM_ (convert dispatchTodoStates) dispatchConvertPaths

convert :: [Text] -> Path Abs File -> IO ()
convert statekeywords path = do
    text <- T.readFile $ toFilePath path
    smosPath <- setFileExtension ".smos" path
    case toSmosFile <$> getDocument statekeywords text of
        Left err -> die err
        Right smosFile -> writeSmosFile smosPath =<< smosFile
