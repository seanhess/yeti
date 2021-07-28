{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Wookie.UI where


import Lucid (Attribute, Html)
import Lucid.Html5 (classes_, div_, span_, style_)
import Lucid.Base (Term(termWith), HtmlT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)

-- ability to add multiple classes
-- can we jump back and forth?
-- erm... not really you have to get into their land


-- this switches to UI Elements, not the other thing!
-- how can we differentiate?
-- just remind them to use it for now

-- classes_ :: [Text] -> Attribute :)


-- these all reduce to attributes, so can I make a new instace?


type ClassName = Text
type Property = Text
data Value
  = Px Int
  | Color Text

data Att
  = Class ClassName
  | Style Property Value
  | Html Attribute


-- | Given attributes, expect more child input.
-- instance (Applicative m) => Term Att (HtmlT m a -> HtmlT m a) where
--   termWith name f = with (makeElement name) . (<> f)
--

class UI arg result | result -> arg where
  ui :: Text -> [Att] -> arg -> result

instance (Applicative m, f ~ HtmlT m a) => UI [ Att ] (f -> HtmlT m a) where
  ui name as1 as2 = termWith name (toAttributes $ as1 <> as2)

instance (Applicative m) => UI (HtmlT m a) (HtmlT m a) where
  ui name as1 = termWith name (toAttributes as1)



row :: UI arg result => arg -> result
row =
  ui "div" [Class "row"]

column :: UI arg result => arg -> result
column =
  ui "div" [Class "column"]

el :: UI arg result => arg -> result
el =
  ui "div" [Class "el"]


-- TODO I feel like this going to come back to bite me
padding :: Int -> Att
padding n = Style "padding" (Px n)

spacing :: Int -> Att
spacing n = Style "gap" (Px n)

-- TODO make better
background :: Text -> Att
background t = Style "background" (Color t)





-- Convert into attributes
toAttributes :: [ Att ] -> [ Attribute ]
toAttributes als =
  let (cls, sys, ats) = splitAtts als
   in classes_ cls : style_ (toStyleAll sys) : ats
  where

    toStyleAll :: [ (Property, Value) ] -> Text
    toStyleAll ss =
      Text.intercalate ";" $ map toStyle ss

    toStyle :: (Property, Value) -> Text
    toStyle (p, v) = p <> ":" <> toValue v

    toValue :: Value -> Text
    toValue (Px n) = cs (show n) <> "px"
    toValue (Color t) = t


    splitAtts :: [ Att ] -> ([ClassName], [(Property, Value)], [Attribute])
    splitAtts as =
      ( mapMaybe attClass as
      , mapMaybe attStyle as
      , mapMaybe attHtml as
      )

    attClass :: Att -> Maybe ClassName
    attClass (Class n) = Just n
    attClass _ = Nothing

    attStyle :: Att -> Maybe (Property, Value)
    attStyle (Style p v) = Just (p, v)
    attStyle _ = Nothing

    attHtml :: Att -> Maybe Attribute
    attHtml (Html a) = Just a
    attHtml _ = Nothing


-- these don't necessarily map to classes
-- toClass :: UI -> Text
-- toClass Row = "row"
-- toClass El = "el"



