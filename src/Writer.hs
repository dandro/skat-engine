{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Writer
-- Description: Write files
--
-- Persist template.
-- @Note: If there are many templates the operation is not atomic.@
module Writer
  ( write,
    WriterError,
    writeDotfile,
  )
where

import Config (GenConfig, dotfileName, outputDirs, separator)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor (($>))
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
  )
import System.IO
  ( Handle,
    IOMode (ReadWriteMode),
    hClose,
    hPutStr,
  )
import System.Path
  ( AbsDir,
    RelDir,
    RelFile,
    absDir,
    relDir,
    relFile,
    toString,
    (</>),
  )
import System.Path.IO (openFile)
import qualified Template as Tpl
import Utils (joinWith, pathStartsWith)

-- | WriterError represents all the possible error states in the Write module
newtype WriterError
  = -- | The process failed to write for some reason. More information should be found in the string.
    FailedToWrite String
  deriving (Show)

mkOutputDir :: AbsDir -> M.Map String RelDir -> String -> AbsDir
mkOutputDir baseDir configOutputDirs templateSourcePath =
  baseDir </> getOutputDir configOutputDirs templateSourcePath
  where
    getOutputDir dirs pathPrefix =
      if null pathPrefix
        then relDir ""
        else
          fromMaybe
            (getOutputDir dirs (take (length pathPrefix - 1) pathPrefix))
            (find (pathStartsWith pathPrefix) dirKeys >>= (`M.lookup` dirs))
    dirKeys = M.keys configOutputDirs

getNameWithExt :: Bool -> Maybe Char -> Tpl.Template -> RelFile
getNameWithExt asModule separator' template =
  relFile $ joinWith [withSeparator separator'] nameParts
  where
    suffix' = Tpl.suffix template
    nameParts =
      if asModule
        then
          [ if null suffix'
              then Tpl.name template
              else suffix',
            Tpl.extension template
          ]
        else [Tpl.name template, suffix', Tpl.extension template]
    withSeparator =
      \case
        Nothing -> '.'
        Just sep -> sep

getFileHandler :: AbsDir -> RelFile -> IO Handle
getFileHandler dirPath filePath = do
  createDirectoryIfMissing shouldCreateParents (toString dirPath)
  openFile (dirPath </> filePath) ReadWriteMode
  where
    shouldCreateParents = True -- TODO: This may need to come from the config instead

persistWithContent :: String -> Handle -> IO ()
persistWithContent content handle = do
  hPutStr handle content
  hClose handle

combineWhenModule :: Bool -> Tpl.Template -> AbsDir -> AbsDir
combineWhenModule asModule template out =
  out
    </> relDir -- TODO: Refactor this to use something like mappend to combine with name if is module
      ( if asModule
          then Tpl.name template
          else ""
      )

-- | Write template file to output directory.
write ::
  -- | Current working directory
  AbsDir ->
  -- | Whether to write the file as a module. If true, it will create a directory and save the templates in it.
  Bool ->
  -- | Config
  GenConfig ->
  -- | Template to write to the output directory
  Tpl.Template ->
  IO (Either WriterError String)
write root asModule config template =
  (getFileHandler outDir filename >>= persistWithContent (Tpl.content template))
    $> Right ("Created: " <> Tpl.name template <> " at " <> toString (outDir </> filename)) -- TODO: Fix this so we can handle errors (Lefts)
  where
    out = mkOutputDir root (outputDirs config) (Tpl.sourcePath template)
    outDir = combineWhenModule asModule template out
    filename = getNameWithExt asModule (separator config) template

-- | Write dotfile in current directory
writeDotfile ::
  -- | Current working directory
  AbsDir ->
  -- | Dotfile contents as JSON string
  BS.ByteString ->
  IO (Either WriterError String)
writeDotfile dir json =
  (getFileHandler dir (relFile ".skatrc") >>= persistWithContent (BS.unpack json))
    $> Right ("Successfully created " <> dotfileName)
