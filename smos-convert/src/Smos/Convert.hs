{-# LANGUAGE RecordWildCards #-}

module Smos.Convert
    ( smosConvert
    ) where

import Import

import Smos.Convert.OptParse

smosConvert :: IO ()
smosConvert = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchConvertFile DispatchConvertFileArgs {..}) _ = convert orgpath

convert :: Path Abs File -> IO ()
convert _ = undefined
