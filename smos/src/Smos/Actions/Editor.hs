{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Actions.Editor
    ( startEditorOnProperties
    , startEditorOnPropertiesAsIs
    , startEditorOnTimestamps
    , startEditorOnTimestampsAsIs
    , startEditorOnLogbook
    , startEditorOnLogbookAsIs
    , startEditorOnContents
    , startEditorOnContentsAsIs
    , startEditorOnTags
    , startEditorOnTagsAsIs
    , startEditorOnYamlAsIs
    , startEditorOnYaml
    , startEditorOnText
    , startEditorOn
    , EditorStart(..)
    , EditorResult(..)
    ) where

import Import

import Data.HashMap.Lazy (HashMap)
import Data.Yaml as Yaml

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T

import System.Exit
import System.Process

import Smos.Data

data EditorStart a = EditorStart
    { editorStartData :: a
    , editorStartContents :: a
    } deriving (Show, Eq, Generic)

instance Functor EditorStart where
    fmap f EditorStart {..} =
        EditorStart
        { editorStartData = f editorStartData
        , editorStartContents = f editorStartContents
        }

data EditorResult a
    = EditorUnchanged
    | EditorChangedTo a
    | EditorError Text -- The resulting contents, because we may want to re-use them
                  Int -- Exit code
                  (Maybe String) -- Error message
    | EditorParsingError Text -- The resulting contents
                         String -- Error message
    deriving (Show, Eq, Generic)

instance Functor EditorResult where
    fmap _ EditorUnchanged = EditorUnchanged
    fmap f (EditorChangedTo a) = EditorChangedTo $ f a
    fmap _ (EditorError t i ms) = EditorError t i ms
    fmap _ (EditorParsingError t s) = EditorParsingError t s

startEditorOnText :: String -> EditorStart Text -> IO (EditorResult Text)
startEditorOnText cmd EditorStart {..} =
    withSystemTempDir "smos" $ \d -> do
        p <- resolveFile d "edit.tmp"
        T.writeFile (toFilePath p) editorStartContents
        let cp =
                (shell $ unwords [cmd, toFilePath p]) -- TODO make this more configurable
                {std_in = NoStream, std_out = NoStream, std_err = NoStream}
        (Nothing, Nothing, Nothing, ph) <- createProcess cp
        c <- waitForProcess ph
        newContents <- T.readFile (toFilePath p)
        pure $
            case c of
                ExitSuccess ->
                    if newContents == editorStartData
                        then EditorUnchanged
                        else EditorChangedTo newContents
                ExitFailure ec ->
                    EditorError newContents ec Nothing -- TODO show a message from stdout?

startEditorOn ::
       (a -> Text)
    -> (Text -> Either String a)
    -> String
    -> EditorStart a
    -> IO (EditorResult a)
startEditorOn inFunc parseFunc cmd start = do
    res <- startEditorOnText cmd $ inFunc <$> start
    pure $
        case res of
            EditorChangedTo t ->
                case parseFunc t of
                    Left err -> EditorParsingError t err
                    Right a -> EditorChangedTo a
            EditorUnchanged -> EditorUnchanged
            EditorError t i ms -> EditorError t i ms
            EditorParsingError t s -> EditorParsingError t s

asIs :: a -> EditorStart a
asIs a = EditorStart {editorStartData = a, editorStartContents = a}

startAsIs ::
       (String -> EditorStart a -> IO (EditorResult a))
    -> String
    -> a
    -> IO (EditorResult a)
startAsIs func cmd = func cmd . asIs

startEditorOnContents ::
       String -> EditorStart Contents -> IO (EditorResult Contents)
startEditorOnContents = startEditorOn contentsText (pure . Contents)

startEditorOnContentsAsIs :: String -> Contents -> IO (EditorResult Contents)
startEditorOnContentsAsIs = startAsIs startEditorOnContents

startEditorOnTags :: String -> EditorStart [Tag] -> IO (EditorResult [Tag])
startEditorOnTags =
    startEditorOn (T.unlines . map tagText) (pure . map Tag . T.lines)

startEditorOnTagsAsIs :: String -> [Tag] -> IO (EditorResult [Tag])
startEditorOnTagsAsIs = startAsIs startEditorOnTags

startEditorOnYaml ::
       (ToJSON a, FromJSON a) => String -> EditorStart a -> IO (EditorResult a)
startEditorOnYaml =
    startEditorOn
        (TE.decodeUtf8 . Yaml.encode)
        (Yaml.decodeEither . TE.encodeUtf8)

startEditorOnYamlAsIs ::
       (ToJSON a, FromJSON a) => String -> a -> IO (EditorResult a)
startEditorOnYamlAsIs = startAsIs startEditorOnYaml

startEditorOnLogbook ::
       String -> EditorStart Logbook -> IO (EditorResult Logbook)
startEditorOnLogbook = startEditorOnYaml

startEditorOnLogbookAsIs :: String -> Logbook -> IO (EditorResult Logbook)
startEditorOnLogbookAsIs = startAsIs startEditorOnLogbook

startEditorOnTimestamps ::
       String
    -> EditorStart (HashMap TimestampName Timestamp)
    -> IO (EditorResult (HashMap TimestampName Timestamp))
startEditorOnTimestamps = startEditorOnYaml

startEditorOnTimestampsAsIs ::
       String
    -> HashMap TimestampName Timestamp
    -> IO (EditorResult (HashMap TimestampName Timestamp))
startEditorOnTimestampsAsIs = startAsIs startEditorOnTimestamps

startEditorOnProperties ::
       String
    -> EditorStart (HashMap PropertyName PropertyValue)
    -> IO (EditorResult (HashMap PropertyName PropertyValue))
startEditorOnProperties = startEditorOnYaml

startEditorOnPropertiesAsIs ::
       String
    -> HashMap PropertyName PropertyValue
    -> IO (EditorResult (HashMap PropertyName PropertyValue))
startEditorOnPropertiesAsIs = startAsIs startEditorOnProperties
