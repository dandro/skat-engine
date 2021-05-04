{-# LANGUAGE LambdaCase #-}

module Main where

import Command (Command (Gen, Init), GenCommand (GenCommand), parserOptions)
import Handlers (handleGenCommand)
import Options.Applicative.Extra (execParser)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = do
  pwd <- getCurrentDirectory -- TODO: Should handle possible errors (This can throw some errors)
  command <- execParser parserOptions
  case command of
    Init -> putStrLn "ERROR: Init not implemented"
    Gen what name asModule sub output -> handleGenCommand pwd (GenCommand what name asModule sub output)
