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
import Juniper.Encode
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




onClick :: (LiveAction action) => action -> Attribute
onClick = on "click"

onInput :: (LiveAction action) => (Text -> action) -> Attribute
onInput = onText "input"

onEnter :: (LiveAction action) => action -> Attribute
onEnter = on "enter"

-- TODO: Always pass the information back the same way.
-- this instance is very easy to manipulate!
-- "{\"contents\":[\"Bob\",true],\"tag\":\"SetCompleted\"}"
onSelect :: (LiveAction action, Input val) => (val -> action) -> Attribute
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

on :: (LiveAction action) => Text -> (action) -> Attribute
on name act = makeAttribute ("data-on-" <> name) $ encodeAction act

onText :: (LiveAction action) => Text -> (Text -> action) -> Attribute
onText name con = onValue name con

onValue :: (LiveAction action, Input val) => Text -> (val -> action) -> Attribute
onValue name con = makeAttribute ("data-on-" <> name) $ encodeAction1 con



data Submit = Submit

instance LiveAction Submit where
  encodeAction' _ = EncodedAction "|Submit|" []

  decodeAction' (EncodedAction "|Submit|" []) = pure Submit
  decodeAction' x = fail $ "Could not parse submit: " <> show x


