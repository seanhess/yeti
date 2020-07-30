{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Runtime where



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


data Message = Message
  { action :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON Message


-- it's either a single or a multi
-- the multi could be described as a list, no?
-- Path x
-- Num y
-- /courses/2|3|4|5

type Key = Text


-- If you put it here, you can't use a typeclass
-- data Path
--   = Root
--   | (:>) Path Segment

-- (</>) :: Path -> Segment -> Path
-- p </> a = p :> a
-- infixl 4 </>


-- (.:) :: Param p => Text -> p -> (Text, Fragment)
-- t .: f = (t, toFragment f)

-- data :> Segment Segment = 

-- Fragments are anything that can be in a URL
-- data Fragment
--   = Str Text
--   | Num Integer
--   | Flag Bool
--   deriving (Show, Eq)
-- instance IsString Fragment where
--   fromString = Str . cs

-- no, this IS the same
newtype Path = Path Value
instance IsString Path where
  fromString = Path . String . cs



parseRoute :: FromJSON r => Text -> Maybe r
parseRoute t = do
  v <- parseTopValue t
  -- Success r <- pure $ fromJSON v
  let res = fromJSON v
  case res of
    Success a -> pure a
    Error e -> error e


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

renderPath :: Path -> Text
renderPath (Path p) = renderValue p



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

  -- = Str Text
  -- | Num Integer
  -- | Flag Bool
  -- | Fields (Map Text Segment)
  -- | Segs [Segment]
  -- deriving (Show, Eq)

-- Maybe this isn't the right level
-- class Param a where
--   toFragment :: a -> Fragment


-- instance Param Integer where
--   toFragment i = Num i

-- instance Param Text where
--   toFragment t = Str t

-- instance Param Segment where toFragment = id

-- instance Param (Map Text Fragment) where
--   toSegment = Fields


-- param :: Param a => a -> Segment
-- param = Single . toFragment

-- fields :: [(Text, Fragment)] -> Segment
-- fields = Fields . Map.fromList

-- so you have a segment, and want to add it to the end


-- -- wait, this will serialize anything
-- instance (ToSegment a) => Segment (Map Text a) where
--   toSegment m = m
--     & Map.toList
--     & map (\(k,v) -> k <> ":" <> toSegment v)
--     & Text.intercalate "|"
--   -- fromSegment t = do
--   --   let fs = Text.splitOn "|" t
--   --   -- if any are nothing this will fail
--   --   ps <- mapM (toPair . Text.splitOn ":")
--   --   pure $ Map.fromList ps
--   --   where
--   --     toPair xs = do
--   --       [k,v] <- xs
--   --       pure (k, v)



-- class Param a where
--   fromParam :: Text -> Maybe a
--   toParam :: a -> Text

-- instance Param Text where
--   fromParam = Just
--   toParam = id

-- instance Param Integer where
--   fromParam = readMaybe . cs
--   toParam = cs . show

-- instance Param a => Param (Maybe a) where
--   fromParam = readMaybe . cs
--   toParam = cs . show



-- class Param a where
  -- fromValue

-- data Param
--   = Str Text
--   | Num Integer
--   deriving (Generic, Show, Eq)


-- renderSegment = undefined

-- renderSegment :: Segment -> Text
-- renderSegment (Path s) = s
-- renderSegment (Fields m) =
--   Map.toList m
--     & map (\(k,v) -> k <> ":" <> v)
--     & Text.intercalate "|"


-- Just use monadfail if it doesn't work
-- class ToSegment a where
--   toSegment :: a -> Segment
--   fromSegment :: MonadFail m => Segment -> m a

data Response = Response
  { resView :: Text
  , resUrl :: Text
  } deriving (Show, Generic)
instance ToJSON Response

-- parseSegment :: Text -> Segment
-- parseSegment t = do
--   -- fromMaybe (Path t) (Num <$> readMaybe (cs t))
--   fromMaybe (Path t) $ (parseNum <|> parseFields <|> parsePath)
--   where
--     parseNum = Num <$> readMaybe (cs t)
--     parseFields = Nothing
--     parsePath = Just $ Path t


-- wait, this needs to run in IO
-- runtime :: forall model action. (Read action, Segment model) => IO model -> (action -> StateT model IO ()) -> (model -> Html ()) -> ByteString -> IO Response
-- runtime load update view body = do
--   m <- load :: IO model
--   let a = read $ cs body :: action
--   m2 <- execStateT (update a) m :: IO model
--   let out = Lucid.renderBS (view m2)
--   pure $ Response (cs out) (renderSegment $ toSegment m2)



data Page params model action = Page
  { load :: params -> IO model
  , update :: action -> StateT model IO ()
  , view :: model -> Html ()
  }
