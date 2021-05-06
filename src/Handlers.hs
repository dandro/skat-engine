{-# LANGUAGE LambdaCase #-}

module Handlers
  ( handleGenCommand,
    handleInitCommand,
  )
where

import Command (GenCommand, InitCommand, asModule, output, what)
import Config
  ( ConfigError,
    Dotfile,
    GenConfig,
    dotfileName,
    handleConfigResult,
    initDotFileToSerializable,
    mkConfig,
    mkDotfile,
    mkInitDotfile,
    parseOutputMap,
    parseSeparator,
    parseTemplatesPath,
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Map.Strict (fromList)
import Data.Monoid (Last (Last))
import System.Console.Haskeline (defaultSettings, runInputT)
import System.Console.Wizard (defaultTo, line, nonEmpty, parser, retryMsg, run)
import System.Console.Wizard.Haskeline (haskeline)
import System.Directory (findFile)
import System.Path (AbsDir, absDir)
import Template (Template, getTemplateFiles)
import Transformations (transformContent)
import Writer (WriterError, write, writeDotfile)

getConfig :: FilePath -> Dotfile -> IO (Either ConfigError GenConfig)
getConfig pwd dotfile =
  findFile [pwd] dotfileName >>= traverse readFile <&> (>>= mkConfig dotfile) <&> handleConfigResult -- TODO: May want to handle findFile Nothing separate to mkConfig Nothing

execWrite :: AbsDir -> GenCommand -> GenConfig -> [Template] -> IO [Either WriterError String]
execWrite root command conf templates =
  traverse (write root (asModule command) conf) (transformContent command <$> templates)

handleGenCommand :: FilePath -> GenCommand -> IO ()
handleGenCommand pwd command =
  getConfig pwd (mkDotfile (Last Nothing) (Last $(\o -> fromList [(what command, o)]) <$> output command) (Last Nothing))
    >>= ( \case
            Left err -> print err
            Right conf ->
              getTemplateFiles (absDir pwd) conf command
                >>= ( \case
                        Left err -> print err
                        Right templates ->
                          execWrite (absDir pwd) command conf templates
                            >>= ( ( \case
                                      Left err -> print err
                                      Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg
                                  )
                                    . sequence
                                )
                    )
        )

handleInitCommand :: FilePath -> InitCommand -> IO ()
handleInitCommand pwd _ = do
  dotfile <- runInputT defaultSettings (run (haskeline $ mkInitDotfile <$> templatePathWizard <*> outputWizard <*> separatorWizard))
  let json = dotfile <&> initDotFileToSerializable <&> encodePretty
  case json of
    Nothing -> putStrLn "ERROR: Could not build dotfile from data entered"
    Just json' ->
      writeDotfile (absDir pwd) json'
        >>= ( \case
                Left err -> print err
                Right msg -> putStrLn msg
            )
  where
    templatePathWizard = retryMsg "Invalid directory path, please try again" $ parser parseTemplatesPath (line "Where are the templates?: ")
    outputWizard =
      retryMsg "Invalid output directory mapping" $
        parser parseOutputMap $
          nonEmpty (line "[optional] Add output mapping separated by a ':' (e.g. component:./components) : ") `defaultTo` "component:./components"
    separatorWizard = parser parseSeparator $ nonEmpty (line "[optional] Spacify a file separator: ") `defaultTo` "."
