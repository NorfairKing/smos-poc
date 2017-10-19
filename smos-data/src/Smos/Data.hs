module Smos.Data
    ( module Smos.Data.Types
    , readSmosFile
    , writeSmosFile
    , emptySmosFile
    ) where

import Import

import qualified Data.ByteString as SB
import Data.Yaml as Yaml

import Smos.Data.Types

readSmosFile :: MonadIO m => Path Abs File -> m (Maybe (Either String SmosFile))
readSmosFile fp = do
    mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath fp
    case mContents of
        Nothing -> pure Nothing
        Just contents -> pure $ Just $ Yaml.decodeEither contents

writeSmosFile :: MonadIO m => Path Abs File -> SmosFile -> m ()
writeSmosFile fp sf =
    liftIO $ do
        ensureDir $ parent fp
        SB.writeFile (toFilePath fp) (Yaml.encode sf)

emptySmosFile :: SmosFile
emptySmosFile = SmosFile $ SmosForest []
