{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Juniper.Events where

import Juniper.Prelude

import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import Data.List as List
import Data.Map as Map (Map, null, empty)
import Data.String.Conversions (cs)
import Data.Text as Text (Text, takeWhile, dropWhile, dropWhileEnd, splitOn)
import Data.Char (isAlphaNum)
import Data.Default (Default(..))
import Juniper.Runtime (Encode, encode, encode1, decode, Encoded(..), LiveAction, Value)
import Lucid.Base (makeAttribute, Attribute)
import Lucid.Html5 (onchange_)


type Name = Text


newtype FormData = FormData (Map Name Text)
  deriving newtype (Monoid, Semigroup)

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




onClick :: (Encode LiveAction action, ToJSON action) => action -> Attribute
onClick = on "click"

onInput :: (Encode LiveAction action, ToJSON action) => (Text -> action) -> Attribute
onInput = onText "input"

onEnter :: (Encode LiveAction action, ToJSON action) => action -> Attribute
onEnter = on "enter"

-- TODO: Always pass the information back the same way.
-- this instance is very easy to manipulate!
-- "{\"contents\":[\"Bob\",true],\"tag\":\"SetCompleted\"}"
onSelect :: (Encode LiveAction action, Value val, ToJSON action) => (val -> action) -> Attribute
onSelect toAction = onValue "select" toAction


-- Oh yeah, I was trying to use JSON for the messages passed back and forth
-- instead of DoSomething {JSON}, instead of straight up read
-- TODO allow for failure here?
-- the idea 
-- oh, it's not serializinug them very well
parseValue :: (Read val) => Text -> val
parseValue s = fromMaybe (error $ "Could not parse value: " <> cs s) $ do
  [tag, vs] <- pure $ Text.splitOn " " s
  let v = Text.dropWhile isQuote . Text.dropWhileEnd isQuote $ vs
  read $ cs $ tag <> " " <> v
  where isQuote = (/= '"')


-- TODO figure out how to cancel it so the page doesn't load
-- onSubmit :: (Encode LiveAction action) => (action) -> Attribute
-- onSubmit = on "submit"


-- we don't need this encode1
-- we want to JSONify things

on :: (Encode LiveAction action, ToJSON action) => Text -> (action) -> Attribute
on name act = makeAttribute ("data-on-" <> name) $ cs $ fromEncoded $ (encode act :: Encoded LiveAction)

onText :: (Encode LiveAction action, ToJSON action) => Text -> (Text -> action) -> Attribute
onText name con = onValue name con

onValue :: (Encode LiveAction action, Value val, ToJSON action) => Text -> (val -> action) -> Attribute
onValue name con = makeAttribute ("data-on-" <> name) $ cs $ fromEncoded $ (encode1 con :: Encoded LiveAction)



data Submit = Submit

instance ToJSON Submit where
  toJSON _ = Aeson.String "|Submit|"
instance FromJSON Submit where
  parseJSON (Aeson.String "|Submit|") = pure Submit
  parseJSON x = fail $ "Could not parse submit: " <> show x

instance Encode LiveAction Submit

