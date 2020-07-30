{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Router where



import Data.String (IsString(..))
import Text.Read (readMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Control.Monad.State.Lazy (StateT, modify, put, execStateT)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), encode, Result(..), fromJSON)
import Data.Aeson (genericToJSON, defaultOptions, Options(sumEncoding), SumEncoding(..), genericParseJSON)
import Data.Aeson.Types (GToJSON, GFromJSON, Zero, Parser)
import Data.Vector as Vector (Vector, toList, fromList)
import Data.Map as Map (Map, toList, fromList)
import Data.HashMap.Strict as HM (HashMap, toList, fromList)
import Data.Text as Text (Text, intercalate, splitOn, drop, dropEnd, stripPrefix, stripSuffix)
import GHC.Generics (Generic, Rep)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)




parseRoute :: FromJSON r => Text -> Maybe r
parseRoute t = do
  v <- parseTopValue t
  Success r <- pure $ fromJSON v
  pure r


-- how??
-- split by "/"
parseTopValue :: Text -> Maybe Value
parseTopValue t = do
  (top:segs) <- pure $ Text.splitOn "/" t
  subs <- mapM parseSegment segs
  pure $ Object $ HM.fromList [(top, Array $ Vector.fromList subs)]


parseSegment :: Text -> Maybe Value
parseSegment s =
  parseBool s <|> parseNumber s <|> parseArray s <|> parseObject s <|> parseString s
  where
    parseString :: Text -> Maybe Value
    parseString t = Just $ String t

    parseNumber :: Text -> Maybe Value
    parseNumber t = Number <$> readMaybe (cs t)

    parseBool :: Text -> Maybe Value
    parseBool "true" = Just $ Bool True
    parseBool "false" = Just $ Bool False
    parseBool _ = Nothing

    parseArray :: Text -> Maybe Value
    parseArray t = do
      t2 <- Text.stripPrefix "[" t
      t3 <- Text.stripSuffix "]" t2
      tv <- pure $ Text.splitOn "," t3
      ss <- mapM parseSegment tv
      pure $ Array $ Vector.fromList ss

    parseObject :: Text -> Maybe Value
    parseObject t = do
      -- it could be only one
      tf <- pure $ Text.splitOn "|" t
      ps <- mapM parsePair tf
      pure $ Object $ HM.fromList ps


    parsePair :: Text -> Maybe (Text, Value)
    parsePair = toPair . Text.splitOn ":"

    toPair :: [Text] -> Maybe (Text, Value)
    toPair xs = do
      [k, v] <- pure xs
      v' <- parseSegment v
      pure (k, v')



renderRoute :: ToJSON a => a -> Text
renderRoute a = renderTopValue $ toJSON a

-- renderPath :: Path -> Text
-- renderPath Root = "/"
-- renderPath (p :> s) = renderPath p <> "/" <> renderSegment s

-- renderPath :: Path -> Text
-- renderPath (Path p) = renderValue p



-- it's going to be an object with a single field, then a list of the other stuffz

renderTopValue :: Value -> Text
renderTopValue (Object o) =
  let fs = HM.toList o
      flat = map flatten fs
  -- take ALL the keys/values
  -- and flatten them with a "/"
  in Text.intercalate "/" $ mconcat flat
  where
    flatten (k, Array v) = k : (map renderValue $ Vector.toList v)
    flatten (k, v) = [k, renderValue v]
renderTopValue v = renderValue v

renderValue :: Value -> Text
renderValue (String s) = s
renderValue (Bool b) = cs $ encode b
renderValue (Number n) = cs $ encode n

renderValue (Array a) =
  let vs = Vector.toList a
      ts = map renderValue vs
  in "[" <> Text.intercalate "," ts <> "]"

-- can I make a whole new object encoding?
-- I don't want quotes. filter out or escape special characters instead
renderValue (Object o) =
  HM.toList o
    & map (\(k,v) -> k <> ":" <> renderValue v)
    & Text.intercalate "|"

renderValue Null = ""


routeToJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
routeToJSON = genericToJSON routeOptions

routeParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
routeParseJSON = genericParseJSON routeOptions

routeOptions :: Options
routeOptions = defaultOptions { sumEncoding = ObjectWithSingleField }
