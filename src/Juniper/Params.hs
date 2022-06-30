
module Juniper.Params where

import Juniper.Prelude
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)
import Data.List as List (lookup)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text

import GHC.Generics



-- encode individual data items
class ToParam param where
  toParam :: param -> Text
  fromParam :: Text -> Maybe param

instance ToParam Text where
  toParam t = t
  fromParam t = Just t

instance ToParam String where
  toParam t = toParam (cs t :: Text)
  fromParam t = cs <$> (fromParam t :: Maybe Text)

instance ToParam Int where
  toParam t = cs $ show t
  fromParam t = readMaybe $ cs t

instance ToParam Day where
  toParam = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  fromParam = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs










class ToParams params where
  encode :: params -> QueryText
  decode :: QueryText -> Maybe params

  default encode :: (Generic params, GenEncode (Rep params)) => params -> QueryText
  encode p = genEncode (from p)

  default decode :: (Generic params, GenEncode (Rep params)) => QueryText -> Maybe params
  decode qs = to <$> genDecode qs

-- The default for simplePage uses () as the params
instance ToParams () where
  encode _ = []
  decode _ = Just ()

-- instance (ToParam a, ToParam b) => ToParams (a, b) where
--   encode (a, b) = [("a", Just $ toParam a), ("b", Just $ toParam b)]

--   -- can I use the generic instance for this?
--   decode qs = Just ()



class GenParam f where
  genEncParam :: f p -> Text
  genDecParam :: Text -> Maybe (f p)

instance (ToParam c) => GenParam (K1 i c) where
  genEncParam (K1 c) = toParam c
  genDecParam t = K1 <$> fromParam t

class GenEncode f where
  genEncode :: f p -> QueryText
  genDecode :: QueryText -> Maybe (f p)

instance (Selector s, GenParam a) => GenEncode (M1 S s a) where
  genEncode x = [(cs $ selName x, Just $ genEncParam (unM1 x))]

  genDecode q = do
    mv <- lookupSel q undefined -- see if there is a query entry that matches our selector name. selName doesn't use the value
    v <- mv                     -- make sure it actually exists
    a <- genDecParam v          -- see if we can decode it, then return
    pure $ M1 a

    where
      lookupSel :: QueryText -> M1 S s a x -> Maybe (Maybe Text)
      lookupSel q' x = List.lookup (cs (selName x)) q'

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


-- instance (ToJSON a, ToJSON b, FromJSON a, FromJSON b) => ToParams (a, b)
-- instance (ToJSON a, ToJSON b, ToJSON c, FromJSON a, FromJSON b, FromJSON c) => ToParams (a, b, c)
-- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON a, FromJSON b, FromJSON c, FromJSON d) => ToParams (a, b, c, d)



