{-# LANGUAGE LambdaCase #-}

module Handlers (
  handleGenCommand,
) where

import Command (GenCommand, asModule, what, output)
import qualified Data.Map.Strict as M
import           Config              (ConfigError, Dotfile, GenConfig,
                                      dotfileName, handleConfigResult, mkConfig,
                                      mkDotfile)
import           System.Path         (AbsDir, absDir, RelDir)
import           Template            (Template, getTemplateFiles)
import           Writer              (WriterError, write)
import           Transformations     (transformContent)
import           System.Directory    (findFile)
import           Data.Functor        ((<&>))
import           Data.Monoid         (Last (Last))
import Data.Map.Strict (fromList)
import           Data.List           (intersperse)

getConfig :: FilePath -> Dotfile -> IO (Either ConfigError GenConfig)
getConfig pwd dotfile =
  findFile [pwd] dotfileName >>= traverse readFile <&> (>>= mkConfig dotfile) <&> handleConfigResult -- TODO: May want to handle findFile Nothing separate to mkConfig Nothing

execWrite :: AbsDir -> GenCommand -> GenConfig -> [Template] -> IO [Either WriterError String]
execWrite root command conf templates =
  traverse (write root (asModule command) conf) (transformContent command <$> templates)

handleGenCommand :: FilePath -> GenCommand -> IO ()
handleGenCommand pwd command =
  getConfig pwd (mkDotfile (Last Nothing) (Last $(\o -> fromList [(what command, o)]) <$> (output command)) (Last Nothing))
  >>= (\case
      Left err -> print err
      Right conf ->
        getTemplateFiles (absDir pwd) conf command >>=
        (\case
           Left err -> print err
           Right templates ->
             execWrite (absDir pwd) command conf templates >>=
             ((\case
                 Left err -> print err
                 Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg) .
              sequence)))