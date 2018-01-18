{-# LANGUAGE RecordWildCards #-}

module Convert
    ( mainFunc
    ) where

import Import

import Convert.OptParse

mainFunc :: IO ()
mainFunc = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchConvertFile DispatchConvertFileArgs {..}) _ = convert orgpath

convert :: Path Abs File -> IO ()
convert _ = undefined
