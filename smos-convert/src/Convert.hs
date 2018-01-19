{-# LANGUAGE RecordWildCards #-}

module Convert
    ( mainFunc
    ) where

import Import

import Convert.Document
import Convert.OptParse
import Convert.SmosFile

import Smos.Data

import qualified Data.Text.IO as T

mainFunc :: IO ()
mainFunc = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchConvertFile DispatchConvertFileArgs {..}) _ = convert orgpath

convert :: Path Abs File -> IO ()
convert path = do
    text <- T.readFile $ toFilePath path
    smosPath <- setFileExtension ".smos" path
    case toSmosFile <$> getDocument text of
        Left err -> die err
        Right smosFile -> writeSmosFile smosPath smosFile
