{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wookie.Events where


import Data.Aeson (encode)

import Data.String.Conversions (cs)
import Data.Text as Text (Text, null)
import Wookie.Page (PageAction(..), stripArgs)
import Lucid.Base (makeAttribute, Attribute)
import Lucid.Html5 (onchange_)
import Data.Map as Map (Map, null, empty)


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




click :: PageAction action => action -> Attribute
click = makeAttribute "data-click" . cs . showAction


-- I don't need this to be generic, I can apply the emptiness myself!
-- submit :: PageAction action => (FormData -> action) -> Attribute
-- submit con = makeAttribute "data-submit" $ cs $ stripArgs $ showAction $ con mempty


-- -- | Submit a form with just one text field (like a search bar)
-- submit1 :: PageAction action => (Value -> action) -> Attribute
-- submit1 con = makeAttribute "data-submit1" $ cs $ stripArgs $ showAction $con mempty


-- onInput :: PageAction action => (Value -> action) -> Attribute
-- onInput con = makeAttribute "data-onInput" $ cs $ stripArgs $ showAction $con mempty

-- is he lowercasing my stuff!?
-- onInput :: PageAction action => (Value -> action) -> Attribute
-- onInput con = makeAttribute "data-input" $ cs $ stripArgs $ showAction $ con mempty

onUpdate :: PageAction action => (Value -> action) -> Attribute
onUpdate con = makeAttribute "data-update" $ cs $ stripArgs $ showAction $ con mempty

onEnter :: PageAction action => action -> Attribute
onEnter = makeAttribute "data-enter" . cs . showAction



data Apply = Apply

instance PageAction Apply where
  showAction _ = "|Apply|"
  readAction "|Apply|" = Just Apply
  readAction _ = Nothing




defaultValue = makeAttribute "data-default-value"


call :: Text -> String -> Text
call fun action = mconcat [fun, "(", cs action, ")"]

