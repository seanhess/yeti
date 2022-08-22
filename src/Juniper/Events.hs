module Juniper.Events where

import Juniper.Prelude

import Data.List as List
import Data.Map as Map (Map, null, empty)
import Data.String.Conversions (cs)
import Data.Text as Text (Text, null, takeWhile)
import Data.Char (isAlphaNum)
import Data.Default (Default(..))
import Juniper.Runtime (Encode, encode, encode1, decode, Encoded(..), LiveAction, Value)
import Lucid.Base (makeAttribute, Attribute)
import Lucid.Html5 (onchange_)


type Name = Text


newtype FormData = FormData (Map Name Text)
  deriving (Monoid, Semigroup)

instance Show FormData where
  -- if it's empty, display it as _
  show (FormData m) =
    if Map.null m
      then "_"
      else show m

-- We don't need this, it'll be read normally
instance Read FormData where
  readsPrec _ "_" = [(FormData $ Map.empty, "")]
  -- I want to map the fst
  readsPrec n s = fmap fstFormData $ readsPrec n s
    where fstFormData (a, s') = (FormData a, s')




onClick :: Encode LiveAction action => action -> Attribute
onClick act = makeAttribute "data-click" . cs . fromEncoded $ (encode act :: Encoded LiveAction)

onInput :: (Encode LiveAction action, Value val) => (val -> action) -> Attribute
onInput con = makeAttribute "data-input" $ cs $ fromEncoded $ (encode1 con :: Encoded LiveAction)

onEnter :: Encode LiveAction action => action -> Attribute
onEnter act = makeAttribute "data-enter" . cs . fromEncoded $ (encode act :: Encoded LiveAction)

onSelect :: (Encode LiveAction action, Value val) => (val -> action) -> Attribute
onSelect con = makeAttribute "data-select" $ cs $ fromEncoded $ (encode1 con :: Encoded LiveAction)


data Submit = Submit
instance Show Submit where
  show _ = "|Submit|"
instance Read Submit where
  readsPrec _ _ = [(Submit, "")]

instance Encode LiveAction Submit

