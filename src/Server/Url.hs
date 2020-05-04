module Server.Url (
  Path,
  UrlParams,
  parseQuery,
  safeParseQuery,
  match,
  matchStrict
) where

import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Maybe (fromMaybe, isJust, fromJust)

type Path = String
type KeyValue = (String, String)
type UrlParams = [KeyValue]
data MatchResult = Param KeyValue | MatchedPath deriving (Show)

makeMatch :: Bool -> Path -> Path -> Maybe UrlParams
makeMatch strict routePath candidate = fmap extractParams matches
  where
    split = splitOn "/"
    match = if strict then matchPathStrict else matchPath
    matches = match (split routePath) (split candidate)

-- /test/:id matches /test/ and /test/:id/next - i.e. there are no length checks
match = makeMatch False

-- /test/:id only matches /test/some-value
matchStrict = makeMatch True

extractParams :: [MatchResult] -> [KeyValue]
extractParams [] = []
extractParams ((Param keyvalue):rest) = keyvalue:(extractParams rest)
extractParams (_:rest) = extractParams rest

matchPath :: [Path] -> [Path] -> Maybe [MatchResult]
matchPath path candidate = traverse matchUrlParts (zip path candidate)

matchPathStrict :: [Path] -> [Path] -> Maybe [MatchResult]
matchPathStrict path candidate = if length path /= length candidate
  then Nothing
  else matchPath path candidate

matchUrlParts :: (Path, Path) -> Maybe MatchResult
matchUrlParts (a, b) = param <|> matchedPath
  where
    key = getParamName a
    value = if b == "" then Nothing else Just b
    param = (\key value -> Param (key, value)) <$> key <*> value
    matchedPath = if a == b then Just MatchedPath else Nothing

getParamName :: String -> Maybe String
getParamName (':':rest) = Just rest
getParamName a = Nothing

parseQuery :: String -> Maybe [KeyValue]
parseQuery queryString = traverse parseParam paramStrings
  where
    queryParamRegex = "^(\\w+)=(\\w+)$" :: String
    paramStrings = splitOn "&" (removeLeadingQuestion queryString)
    parseParam = (\param -> getKeyValue (param =~ queryParamRegex))

removeLeadingQuestion :: String -> String
removeLeadingQuestion ('?':rest) = rest
removeLeadingQuestion a = a

getKeyValue :: [[String]] -> Maybe KeyValue
getKeyValue [[_, key, value]] = Just (key, value)
getKeyValue _ = Nothing

safeParseQuery = (fromMaybe []) . parseQuery
