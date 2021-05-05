{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Config
-- Description: Application configuration
--
-- Contains metadata for the application as well as values needed for SKAT to run.
module Config
  ( GenConfig (templatesDir, outputDirs, separator),
    mkConfig,
    mkDotfile,
    dotfileName,
    handleConfigResult,
    Dotfile,
    ConfigError,
    InitDotfile (InitDotfile),
    parseTemplatesPath,
    parseOutputMap,
    parseSeparator,
    mkInitDotfile,
    InitDotfileSerializable,
    initDotFileToSerializable,
  )
where

import Data.Aeson (decode)
import Data.Aeson.Types
  ( FromJSON,
    Parser,
    ToJSON,
    object,
    parseJSON,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.ByteString.Lazy.Char8 as LazyC
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (Last), getLast)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Path
  ( Dir,
    RelDir,
    parse,
    relDir,
    toString,
  )
import System.Path.Generic ((</>))
import Utils (mkPair, upperCase)

-- |
--  It represents the data coming from the configuration file. More data can be needed but may not be configurable.
--  The Dotfile is used to create a GenConfig which will be used by the application.
data Dotfile = Dotfile
  { -- | Templates directory path relative to the root.
    templates' :: Last RelDir,
    -- | Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    output' :: Last (M.Map String RelDir),
    -- | Character used to separate the template filename and make use of suffix.
    filenameSeparator' :: Last (Maybe Char)
  }
  deriving (Generic, Show)

-- |
--  Encompasses all possible errors in the Config module
newtype ConfigError
  = -- | Cannot create a valid config from all inputs
    CouldNotMakeConfig String
  deriving (Show)

-- |
--  Transform maybe config to an Either of the correct Domain error or the valid Config
handleConfigResult :: Maybe GenConfig -> Either ConfigError GenConfig
handleConfigResult (Just config) = Right config
handleConfigResult Nothing = Left $ CouldNotMakeConfig "ERROR: Coud not make a valid configuration."

mkDirPath :: (String -> Dir os) -> T.Text -> Last (Dir os)
mkDirPath toPath = Last . Just . toPath . T.unpack

mkOutputDirs :: M.Map String String -> Last (M.Map String RelDir)
mkOutputDirs m = Last $ Just (M.foldrWithKey (\k v result -> M.insert k (relDir v) result) M.empty m)

instance FromJSON Dotfile where
  parseJSON =
    withObject
      "Dotfile"
      ( \o -> do
          templates' <- mkDirPath relDir <$> (o .: "templates" :: Parser T.Text)
          output' <- mkOutputDirs <$> (o .: "output" :: Parser (M.Map String String))
          filenameSeparator' <- Last <$> o .:? "filenameSeparator"
          return $ mkDotfile templates' output' filenameSeparator'
      )

instance Semigroup Dotfile where
  a <> b =
    Dotfile
      { templates' = templates' a <> templates' b,
        output' = output' a <> output' b,
        filenameSeparator' = filenameSeparator' a <> filenameSeparator' b
      }

-- |
--  This is the configuration used by the application wtih all the metadata needed to generate code.
data GenConfig = GenConfig
  { -- | Templates directory path relative to the root.
    templatesDir :: RelDir,
    -- | Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
    outputDirs :: M.Map String RelDir,
    -- | Character used to separate the template filename and make use of suffix.
    separator :: Maybe Char
  }
  deriving (Show, Eq)

-- | Name for the dotfile where the configuration will be read from
dotfileName :: String
dotfileName = ".skatrc"

makeDefaultConfig :: Dotfile
makeDefaultConfig = Dotfile (Last Nothing) (Last Nothing) (Last $ Just Nothing)

emptyConfigOption :: Dotfile
emptyConfigOption = Dotfile (Last Nothing) (Last Nothing) (Last Nothing)

decodeConfig :: String -> Dotfile
decodeConfig content = fromMaybe emptyConfigOption (decode (LazyC.pack content) :: Maybe Dotfile)

-- |
--  Factory function for constructing a Dotfile
mkDotfile ::
  -- | Templates directory path relative to the root.
  Last RelDir ->
  -- | Configuration for the output. Keys are matched on the names of the templates, the values are relative directory paths inside the root.
  Last (M.Map String RelDir) ->
  -- | Character used to separate the template filename and make use of suffix.
  Last (Maybe Char) ->
  Dotfile
mkDotfile = Dotfile

-- |
--  Factory to make a GenConfig from a JSONString. It will attempt to construct a Dotfile from its values
--  and then combine it with a default config. The result will be the GenConfig that will be returned.
--
--  >>> mkConfig "{ \"root\": \"/dummy/project\", \"templates\": \".skat/templates\", \"filenameSeparator\": \".\", \"output\": { \"component\": \"./components\" }}"
mkConfig :: Dotfile -> String -> Maybe GenConfig
mkConfig dotfile content =
  GenConfig <$> getLast (templates' configOption) <*> getLast (output' configOption)
    <*> getLast (filenameSeparator' configOption)
  where
    configOption = makeDefaultConfig <> decodeConfig content <> dotfile

data InitDotfile = InitDotfile
  { templatesVal :: RelDir,
    outputVal :: M.Map String RelDir,
    filenameSeparatorVal :: Maybe Char
  }
  deriving (Show)

data InitDotfileSerializable = InitDotfileSerializable
  { templates :: String,
    output :: M.Map String String,
    filenameSeparator :: String
  }
  deriving (Generic, Show, ToJSON)

initDotFileToSerializable :: InitDotfile -> InitDotfileSerializable
initDotFileToSerializable (InitDotfile t o s) = InitDotfileSerializable (toString t) (M.map toString o) sep
  where
    sep = case s of
      Nothing -> ""
      Just c -> [c]

mkInitDotfile :: RelDir -> M.Map String RelDir -> Maybe Char -> InitDotfile
mkInitDotfile = InitDotfile

parseTemplatesPath :: String -> Maybe RelDir
parseTemplatesPath input = case parse input of
  Left _ -> Nothing
  Right path -> Just path

parseOutputMap :: String -> Maybe (M.Map String RelDir)
parseOutputMap input =
  ( \case
      Left _ -> Nothing
      Right kvPair -> Just $ M.fromList [kvPair]
  )
    (mkPair pair >>= (\(k, v) -> parse v <&> (k,)))
  where
    pair = LazyC.unpack <$> LazyC.split ':' (LazyC.pack input)

parseSeparator :: String -> Maybe (Maybe Char)
parseSeparator [c] = Just . Just $ c
parseSeparator _ = Nothing
