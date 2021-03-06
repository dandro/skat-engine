{-# LANGUAGE LambdaCase #-}

-- |
-- Module : Template
-- Description: Files to be written
--
-- Templates represent the file to be written while holding information from the source template.
module Template
  ( Template (name, suffix, content, extension, sourcePath),
    mkTemplate,
    getTemplateFiles,
    TemplateError,
  )
where

import qualified Command as Comm
import Config (GenConfig, separator, templatesDir)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<&>))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Path
  ( AbsDir,
    absDir,
    relFile,
    toString,
    (</>),
  )
import Utils (joinWith, pathStartsWith)

-- | A Template represents the file that is going to be written. It
-- contains the content from the @source template@ as well as the
-- @source path@ which is the filename of the @source template@
data Template = Template
  { -- | Name of the Template. The name comes from the Command.
    name :: String,
    -- | Suffix for the file name. It comes from the source path and it is identified when the source path is split by rhe filenameSeparator. It is also optional.
    suffix :: String,
    -- | Actual contents of the template.
    content :: String,
    -- | Filename extension.
    extension :: String,
    -- | Original filename of the template.
    sourcePath :: String
  }
  deriving (Show, Eq)

-- |
--  Encompasses all possible errors in the Template module
newtype TemplateError
  = NoMatchFound String
  deriving (Show)

toTemplate :: Maybe Char -> String -> (String, String) -> Template
toTemplate Nothing name (path, content) = Template name "" content (ext '.' path) path
toTemplate (Just separator') name (path, content) =
  Template name (mkSuffix separator' path) content (ext separator' path) path

-- |
--  Use the filename separator to find and return the @suffix@ for the template.
--  If there is no suffix the function returns an empty string.
mkSuffix :: Char -> String -> String
mkSuffix separator' path = findSuffix $ map BS.unpack $ BS.split separator' (BS.pack path)
  where
    findSuffix xs =
      if hasSuffix xs
        then joinWith [separator'] $ take (length xs - 2) (tail xs)
        else ""
    hasSuffix xs = length xs > 2

ext :: Char -> String -> String
ext separator' path = BS.unpack $ last $ BS.split separator' (BS.pack path)

-- | Template factory
mkTemplate :: String -> String -> String -> String -> String -> Template
mkTemplate = Template

-- |
--  Iterates through all files in SKAT template's source directory.
--  For each file in the directory it will match all files that start
--  with the `what` passed in the command.
getTemplateFiles :: AbsDir -> GenConfig -> Comm.GenCommand -> IO (Either TemplateError [Template])
getTemplateFiles root config command =
  doesDirectoryExist (toString templatesPath)
    >>= ( \case
            True ->
              listDirectory (toString templatesPath)
                >>= ( \paths -> do
                        contents <- traverse (readFile . toString . (</>) templatesPath) (relFile <$> paths) -- TODO: Handle error when file does not exist
                        pure $ zip paths contents
                    )
                  . filter pred
            False -> pure []
        )
    <&> (<$>) (toTemplate (separator config) (Comm.name command))
    <&> ( \case
            [] -> Left $ NoMatchFound "Did not find any matching templates."
            templates -> Right templates
        )
  where
    templatesPath = root </> templatesDir config
    pred = pathStartsWith (Comm.what command)
