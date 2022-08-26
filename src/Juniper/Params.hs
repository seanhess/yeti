{-# LANGUAGE InstanceSigs #-}
module Juniper.Params where

import Juniper.Prelude
import Data.List as List (lookup)
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics
import Network.HTTP.Types.URI (QueryText)
import Network.URI (isUnreserved, escapeURIString)
import Network.URI.Encode (encodeTextWith, decodeText)
import Numeric (showFFloat)
import Text.Read (readMaybe)
import qualified Data.Text as Text



-- encode individual data items
class ToParam param where
  toParam :: param -> Text
  fromParam :: Text -> Maybe param

  default toParam :: (Show param) => param -> Text
  toParam = cs . show

  default fromParam :: (Read param) => Text -> Maybe param
  fromParam = readMaybe . cs

-- we should url escape these?, maybe it already happens
instance ToParam Text where
  toParam t = t
  fromParam t = Just t

instance ToParam Int where
  toParam t = cs $ show t
  fromParam t = readMaybe $ cs t

instance ToParam Bool where
  toParam True = "1"
  toParam False = "0"
  fromParam "1" = Just True
  fromParam "0" = Just False
  fromParam _ = Nothing

instance ToParam Integer where
  toParam t = cs $ show t
  fromParam t = readMaybe $ cs t

instance ToParam Float where
  toParam n = cs $ showFFloat (Just 2) n ""
  fromParam t = readMaybe $ cs t

instance ToParam Day where
  toParam = cs . formatTime defaultTimeLocale "%Y-%m-%d"
  fromParam = parseTimeM True defaultTimeLocale "%Y-%m-%d" . cs

instance ToParam a => ToParam (Maybe a) where
  toParam (Just a) = toParam a
  toParam Nothing  = ""

  fromParam :: Text -> Maybe (Maybe a)
  fromParam t =
    case (fromParam t :: Maybe a) of
      Nothing -> Nothing
      Just a -> Just (Just a)

-- TODO escape toParam now, what if it has my character in it?
instance ToParam a => ToParam [a] where
  -- we ONLY need to encode the "," character
  toParam = Text.intercalate "," . map (escapeChar ',' . toParam)

  fromParam "" = Just []
  fromParam t = mapM (fromParam . urlDecode) . Text.splitOn "," $ t

instance ToParam b => ToParam (Tagged a b) where
  toParam (Tagged b) = toParam b
  fromParam t = Tagged <$> fromParam t

instance (ToParam a, ToParam b) => ToParam (a, b) where
  -- we ONLY need to encode the "-" character
  toParam (r, c) = esc (toParam r) <> "-" <> esc (toParam c)
    where esc = escapeChar '-'

  fromParam t = do
    [tr, tc] <- pure $ Text.splitOn "-" t
    r <- fromParam $ urlDecode tr
    c <- fromParam $ urlDecode tc
    pure (r, c)

instance (ToParam a, ToParam b, ToParam c) => ToParam (a, b, c) where
  -- we ONLY need to encode the "-" character
  toParam (a, b, c) = Text.intercalate "-" $ map esc [toParam a, toParam b, toParam c]
    where esc = escapeChar '|'

  fromParam t = do
    [ta, tb, tc] <- pure $ Text.splitOn "-" t
    a <- fromParam $ urlDecode ta
    b <- fromParam $ urlDecode tb
    c <- fromParam $ urlDecode tc
    pure (a, b, c)


escapeChar :: Char -> Text -> Text
escapeChar c = cs . escapeURIString (/=c) . cs

urlDecode :: Text -> Text
urlDecode =
  decodeText

urlEncode :: Text -> Text
urlEncode =
  encodeTextWith skipEscape

-- all the chars we use we want to pass through
skipEscape :: Char -> Bool
-- skipEscape '+' = True
skipEscape ',' = True
skipEscape '-' = True
-- skipEscape '|' = True
skipEscape c = isUnreserved c
    







class ToParams params where
  toParams :: params -> QueryText
  fromParams :: QueryText -> Maybe params

  default toParams :: (Generic params, GenEncode (Rep params)) => params -> QueryText
  toParams p = genEncode (from p)

  default fromParams :: (Generic params, GenEncode (Rep params)) => QueryText -> Maybe params
  fromParams qs = to <$> genDecode qs

instance ToParams () where
  -- The default for simplePage uses () as the params
  toParams _ = []
  fromParams _ = Just ()



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
  genEncode x = 
    case selName x of
      "" -> []
      n -> [(cs n, Just $ genEncParam (unM1 x))]

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



