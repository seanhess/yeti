module Juniper.Events where

import Juniper.Prelude

import Data.List as List
import Data.Map as Map (Map, null, empty)
import Data.String.Conversions (cs)
import Data.Text as Text (Text, null, dropWhileEnd)
import Juniper.Runtime (Encode(..), Encoded(..), LiveAction)
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



newtype Value = Value Text
  deriving (Monoid, Semigroup)

instance Show Value where
  show (Value t) =
    if Text.null t
      then "_"
      else show t

instance Read Value where
  readsPrec _ "_" = [(Value "", "")]
  readsPrec n s = fmap fstFormData $ readsPrec n s
    where fstFormData (a, s') = (Value a, s')




onClick :: Encode LiveAction action => action -> Attribute
onClick act = makeAttribute "data-click" . cs . fromEncoded $ (encode act :: Encoded LiveAction)

onInput :: Encode LiveAction action => (Value -> action) -> Attribute
onInput con = makeAttribute "data-input" $ cs $ fromEncoded $ stripArgs $ (encode $ con mempty :: Encoded LiveAction)

onEnter :: Encode LiveAction action => action -> Attribute
onEnter act = makeAttribute "data-enter" . cs . fromEncoded $ (encode act :: Encoded LiveAction)

-- -- | Only fires when the element loses focus
-- onChange :: PageAction action => action -> Attribute
-- onChange = makeAttribute "data-change" . cs . showAction



data Submit = Submit

instance Encode LiveAction Submit where
  encode _ = Encoded "|Submit|"
  decode (Encoded "|Submit|") = Just Submit
  decode _ = Nothing


call :: Text -> String -> Text
call fun action = mconcat [fun, "(", cs action, ")"]



stripArgs :: Encoded a -> Encoded a
stripArgs (Encoded t) = Encoded $ Text.dropWhileEnd isArg t
  where isArg c = c == ' ' || c == '_'

