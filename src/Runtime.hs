{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (ToJSON, FromJSON(..), Value(..))
import Data.Map as Map (Map, toList, fromList)
import Data.Text as Text (Text, intercalate, splitOn)
import GHC.Generics (Generic)
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
data Path
  = Root
  | (:>) Path Segment

(</>) :: Path -> Segment -> Path
p </> a = p :> a
infixl 4 </>


(.:) :: Param p => Text -> p -> (Text, Fragment)
t .: f = (t, toFragment f)

-- data :> Segment Segment = 

-- Fragments are anything that can be in a URL
data Fragment
  = Str Text
  | Num Integer
  | Flag Bool
  deriving (Show, Eq)
instance IsString Fragment where
  fromString = Str . cs

data Segment
  = Single Fragment
  | Fields (Map Text Fragment)
  | Multi [Fragment]
  deriving (Show, Eq)
instance IsString Segment where
  fromString = Single . fromString

-- Maybe this isn't the right level
class Param a where
  toFragment :: a -> Fragment


instance Param Integer where
  toFragment i = Num i

instance Param Text where
  toFragment t = Str t

-- instance Param Segment where toFragment = id

-- instance Param (Map Text Fragment) where
--   toSegment = Fields


param :: Param a => a -> Segment
param = Single . toFragment

fields :: [(Text, Fragment)] -> Segment
fields = Fields . Map.fromList

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


renderSegment = undefined

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
