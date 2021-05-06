{-# LANGUAGE LambdaCase #-}

module Handlers
  ( handleGenCommand,
    handleInitCommand,
    handleExploreCommand,
  )
where

import Command (ExploreCommand, GenCommand, InitCommand, asModule, name, output, what)
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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Default (def)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Map.Strict (fromList)
import Data.Monoid (Last (Last))
import System.Console.Haskeline (defaultSettings, runInputT)
import System.Console.StructuredCLI (Action (NewLevel, NoAction), Commands, CommandsT, Validator, command, exit, getBanner, param, runCLI, (>+))
import System.Console.Wizard (defaultTo, line, nonEmpty, parser, retryMsg, run)
import System.Console.Wizard.Haskeline (haskeline)
import System.Directory (findFile)
import System.Exit (die)
import System.Path (AbsDir, absDir)
import Template (Template, getTemplateFiles, sourcePath)
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
            Left err -> (die . show) err
            Right conf ->
              getTemplateFiles (absDir pwd) conf (name command) (what command)
                >>= ( \case
                        Left err -> (die . show) err
                        Right templates ->
                          execWrite (absDir pwd) command conf templates
                            >>= ( ( \case
                                      Left err -> (die . show) err
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
    Nothing -> die "Could not build dotfile from data entered"
    Just json' ->
      writeDotfile (absDir pwd) json'
        >>= ( \case
                Left err -> (die . show) err
                Right msg -> putStrLn msg
            )
  where
    templatePathWizard = retryMsg "Invalid directory path, please try again" $ parser parseTemplatesPath (line "Where are the templates?: ")
    outputWizard =
      retryMsg "Invalid output directory mapping" $
        parser parseOutputMap $
          nonEmpty (line "[optional] Add output mapping separated by a ':' (e.g. component:./components, action:./store/actions) : ") `defaultTo` "component:./components"
    separatorWizard = parser parseSeparator $ nonEmpty (line "[optional] Spacify a file separator: ") `defaultTo` "."

explorer :: AbsDir -> GenConfig -> Commands ()
explorer pwd config = do
  listMatchedTemplates pwd config

instructions :: String
instructions = "\nExplorer Usage Instructions\nhelp: ? | exit: ctrl + c | auto-complete: tab\n"

listMatchedTemplates :: AbsDir -> GenConfig -> CommandsT IO ()
listMatchedTemplates pwd config =
  param
    "search"
    "Search usage: type 'search' followed by the name of the templates you'd want to match. e.g. search component "
    (return . return)
    handleSearch
    >+ do
      listTemplateVars config
      command "back" "Search again" exit
  where
    handleSearch input = do
      results <- getTemplateFiles pwd config "" input
      liftIO $
        ( \case
            Left err -> print err
            Right msg -> putStrLn $ foldr (<>) "" $ intersperse "\n" msg
        )
          $ results <&> map sourcePath
      putStrLn instructions
      return NewLevel

listTemplateVars :: GenConfig -> Commands ()
listTemplateVars _config = command "list-vars" "List template variables" $ do
  liftIO . putStrLn $ "This happened"
  putStrLn instructions
  return NoAction

handleExploreCommand :: FilePath -> ExploreCommand -> IO ()
handleExploreCommand pwd _ = do
  config <- getConfig pwd (mkDotfile (Last Nothing) (Last Nothing) (Last Nothing))
  case config of
    Left err -> print err
    Right conf -> void $ runCLI "Explore your SKAT templates" settings $ explorer (absDir pwd) conf
  where
    settings = def {getBanner = "Welcome to SKAT explorer.\n" <> instructions}
