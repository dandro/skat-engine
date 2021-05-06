module Main where

import Command (Command (Explore, Gen, Init), mkExploreCommand, mkGenCommand, mkInitCommand, parserOptions)
import Handlers (handleExploreCommand, handleGenCommand, handleInitCommand)
import Options.Applicative.Extra (execParser)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  pwd <- getCurrentDirectory -- TODO: Should handle possible errors (This can throw some errors)
  command <- execParser parserOptions
  case command of
    Init -> handleInitCommand pwd mkInitCommand
    Gen what name asModule sub output -> handleGenCommand pwd (mkGenCommand what name asModule sub output)
    Explore -> handleExploreCommand pwd mkExploreCommand
