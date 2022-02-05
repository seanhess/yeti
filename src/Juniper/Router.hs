{-# LANGUAGE OverloadedStrings #-}
module Juniper.Router
  ( Path(..)
  , RoutePath(..)
  , renderPath
  , parsePath
  , parseSegment
  , fields, (.:)
  , field
  ) where


import Data.String (IsString(..))
import Text.Read (readMaybe)
import Data.Function ((&))
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Map as Map (Map, toList, fromList, lookup)
import Data.Text as Text (Text, intercalate, splitOn, stripPrefix, stripSuffix)



class RoutePath a where
  toPath :: a -> Path
  fromPath :: Path -> Maybe a

data Path
  = Str Text
  | Num Integer
  | Flag Bool
  | Fields (Map Text Path)
  | Array [Path]
  | (:>) Path Path
  deriving (Show, Eq)
instance IsString Path where
  fromString = Str . cs

fields :: [(Text, Path)] -> Path
fields = Fields . Map.fromList

(.:) :: Text -> Path -> (Text, Path)
t .: p = (t, p)

renderPath :: Path -> Text
renderPath (Str t) = t
renderPath (Num n) = cs $ show n
renderPath (Flag True) = "true"
renderPath (Flag False) = "false"
renderPath (p1 :> p2) = renderPath p1 <> "/" <> renderPath p2
renderPath (Fields m) =
  Map.toList m
    & map (\(k,v) -> k <> ":" <> renderPath v)
    & Text.intercalate "|"
renderPath (Array ps) =
  let ts = map renderPath ps
  in "[" <> Text.intercalate "," ts <> "]"


-- well, wait a minute, we're just piping it all through?
parsePath :: Text -> Maybe Path
parsePath t = do
  let ss = Text.splitOn "/" t
  ps <- mapM parseSegment ss
  pure $ foldl1 ((:>)) ps



parseSegment :: Text -> Maybe Path
parseSegment s =
  parseBool s <|> parseNumber s <|> parseObject s <|> parseArray s <|> parseString s
  where
    parseString :: Text -> Maybe Path
    parseString t = Just $ Str t

    parseNumber :: Text -> Maybe Path
    parseNumber t = Num <$> readMaybe (cs t)

    parseBool :: Text -> Maybe Path
    parseBool "true" = Just $ Flag True
    parseBool "false" = Just $ Flag False
    parseBool _ = Nothing

    parseObject :: Text -> Maybe Path
    parseObject t = do
      tf <- pure $ Text.splitOn "|" t
      ps <- mapM parsePair tf
      pure $ Fields $ Map.fromList ps

    parseArray :: Text -> Maybe Path
    parseArray t = do
      t2 <- Text.stripPrefix "[" t
      t3 <- Text.stripSuffix "]" t2
      tv <- pure $ Text.splitOn "," t3
      Array <$> mapM parseSegment tv


    parsePair :: Text -> Maybe (Text, Path)
    parsePair = toPair . Text.splitOn ":"

    toPair :: [Text] -> Maybe (Text, Path)
    toPair xs = do
      [k, v] <- pure xs
      v' <- parseSegment v
      pure (k, v')


field :: Text -> Map Text Path -> Maybe Path
field = Map.lookup
