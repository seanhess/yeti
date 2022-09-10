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
onInput = onValue "text-input"

-- | capture the input event and commit immediately. Should not be used for text inputs
onSelect :: (LiveAction action, Input val) => (val -> action) -> Attribute
onSelect = onValue "input"

onEnter :: (LiveAction action) => action -> Attribute
onEnter = on "enter"



on :: (LiveAction action) => Text -> (action) -> Attribute
on name act = makeAttribute ("data-on-" <> name) $ encodeAction act

onValue :: (LiveAction action, Input val) => Text -> (val -> action) -> Attribute
onValue name con = makeAttribute ("data-on-" <> name) $ encodeAction1 con




data Submit = Submit

instance LiveAction Submit where
  encodeAction' _ = EncodedAction "|Submit|" []

  decodeAction' (EncodedAction "|Submit|" []) = pure Submit
  decodeAction' x = fail $ "Could not parse submit: " <> show x


