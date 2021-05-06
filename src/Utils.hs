-- |
-- Module: Utils
-- Description: Utility functions
--
-- Collection of generic utility functions.
module Utils
  ( joinWith,
    upperCase,
    trim,
    pathStartsWith,
    mkPair,
    hasEmptyStrings,
    toListOfStr,
    distinctMapFromList,
  )
where

import Control.Arrow (left)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Join a collection of strings with a separator
joinWith ::
  -- | Separator to join strings with
  String ->
  -- | Collections of strings to join
  [String] ->
  String
joinWith separator strings =
  BS.unpack $ BS.intercalate (BS.pack separator) (filter (not . BS.null) (BS.pack <$> strings))

-- | Trim start and end of word. It uses Text.strip under the hood.
trim :: String -> String
trim str = T.unpack $ T.strip $ T.pack str

-- | Transform word to upper case
upperCase :: String -> String
upperCase = T.unpack . T.toUpper . T.pack

-- | Check if string starts with another string
pathStartsWith :: String -> FilePath -> Bool
pathStartsWith str path = BS.isPrefixOf (BS.pack str) (BS.pack path)

hasEmptyStrings :: [String] -> Bool
hasEmptyStrings = any null

mkPair :: [String] -> Either String (String, String)
mkPair pair =
  if length pair /= 2 || hasEmptyStrings pair
    then Left "The input is not a pair"
    else Right (head pair, last pair)

-- |
--  This function is meant to split the str by ','
--  first and then split each result by ':' and
--  then trim each string in the inner List. The
--  IDE helped format it this way but it's a tad hard
--  to grok.
--  e.g.
--       if str is "$A$:a, $B$:b"
--       then the result is [["A", "a"], ["B", "b"]]
toListOfStr :: String -> [[String]]
toListOfStr str =
  map (map (trim . T.unpack) . T.splitOn (T.pack ":")) $
    T.splitOn (T.pack ",") $ T.pack str

safeInsert ::
  ((String, String) -> M.Map String String -> e) ->
  (String, String) ->
  M.Map String String ->
  Either e (M.Map String String)
safeInsert mkErr pair m =
  if M.member (fst pair) m
    then Left $ mkErr pair m
    else Right $ uncurry M.insert pair m

distinctMapFromList ::
  ([String] -> e) ->
  ((String, String) -> M.Map String String -> e) ->
  [[String]] ->
  Either e (M.Map String String)
distinctMapFromList mkInvalidPairError mkDuplicateError listOfPairs =
  traverse parsePair listOfPairs >>= foldr (\v acc -> acc >>= safeInsert mkDuplicateError v) (Right M.empty)
  where
    parsePair pair = left (const $ mkInvalidPairError pair) $ mkPair pair
