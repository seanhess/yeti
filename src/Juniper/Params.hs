
module Juniper.Params where

import Juniper.Prelude
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)
import Data.List as List (lookup)
import Data.Proxy (Proxy(..))

import GHC.Generics

-- TODO use generics directly instead of JSON
-- TODO doesn't handle spaces. Do we need to use base64 for this too? Or... just fix it so it doesn't url encode on the way down

-- does it always serialize to a querystring?
-- what if you wanted to use paths?
class ToParams params where
  encode :: params -> QueryText
  decode :: QueryText -> Maybe params

  default encode :: (Generic params, GenEncode (Rep params)) => params -> QueryText
  encode p = genEncode (from p)

  default decode :: (Generic params, GenEncode (Rep params)) => QueryText -> Maybe params
  decode qs = to <$> genDecode qs



-- encode individual data items
class ToParam param where
  toParam :: param -> Text
  fromParam :: Text -> Maybe param

instance ToParam Text where
  toParam t = t
  fromParam t = Just t

instance ToParam String where
  toParam t = cs t
  fromParam t = Just (cs t)

instance ToParam Int where
  toParam t = cs $ show t
  fromParam t = readMaybe $ cs t




class GenParam f where
  genEncParam :: f p -> Text
  genDecParam :: Text -> Maybe (f p)

instance (ToParam c) => GenParam (K1 i c) where
  genEncParam (K1 c) = toParam c
  genDecParam t = K1 <$> fromParam t

class GenEncode f where
  genEncode :: f p -> QueryText

  -- and how do you know if you can instantiate all of them?
  -- if we pattern match all the decodes and get what we need

  genDecode :: QueryText -> Maybe (f p)

-- can we get one of our things from this? 
-- we don't have a value yet
instance (Selector s, GenParam a) => GenEncode (M1 S s a) where
  genEncode x = [(cs $ selName x, Just $ genEncParam (unM1 x))]

  genDecode q = do
    -- see if there is a query entry that matches our selector name
    mv <- lookupSel q (selName (undefined :: M1 S s a x))
    -- make sure it actually exists
    v <- mv

    -- see if we can decode it, then return
    a <- genDecParam v
    pure $ M1 a

    where
      lookupSel :: QueryText -> String -> Maybe (Maybe Text)
      lookupSel q' sel = List.lookup (cs sel) q'

      toVal :: GenParam f => (String, Text) -> (Text, Maybe (f p))
      toVal (k, t) = (cs k, genDecParam t)

instance GenEncode f => GenEncode (M1 D d f) where
  genEncode (M1 x) = genEncode x
  genDecode q = M1 <$> genDecode q

instance GenEncode f => GenEncode (M1 C c f) where
  genEncode (M1 x) = genEncode x
  genDecode q = M1 <$> genDecode q

-- this is a sum type, meaning either this constructor or that
instance (GenEncode a, GenEncode b) => GenEncode (a :+: b) where
  genEncode (L1 x) = genEncode x
  genEncode (R1 x) = genEncode x
  genDecode q = L1 <$> genDecode q <|> R1 <$> genDecode q

instance (GenEncode a, GenEncode b) => GenEncode (a :*: b) where
  genEncode (a :*: b) = genEncode a ++ genEncode b

  genDecode q = do
    a <- genDecode q
    b <- genDecode q
    pure $ a :*: b

data Test = Test { test1 :: Int, test2 :: String }
  deriving (Generic, Show, Eq)
instance ToParams Test

test = Test 1 "asdf"

-- instance ToParams Day where
--   encode = cs . formatTime defaultTimeLocale "%Y-%m-%d"
--   decode = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

-- class Selectors rep where
--   selectors :: [String]

-- genEncode :: Generic a => a -> QueryText
-- genEncode = _


-- fields' :: (Generic a, GenEncode (Rep a)) => a -> [String]
-- fields' = genFields . from


instance ToParams () where
  encode _ = []
  decode _ = Just ()


-- instance (ToJSON a, ToJSON b, FromJSON a, FromJSON b) => ToParams (a, b)
-- instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => ToParams (a, b, c)
-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ToParams (a, b, c, d)



