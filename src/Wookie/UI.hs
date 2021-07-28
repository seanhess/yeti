{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Wookie.UI where


import Lucid (Attribute, Html)
import Lucid.Html5 (classes_, div_, span_, style_, rel_)
import Lucid.Base (Term(termWith), HtmlT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

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
  = Pxv Int
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
  ui name as1 as2 = termWith name (toAttributes $ Class "a" : as1 <> as2)

instance (Applicative m) => UI (HtmlT m a) (HtmlT m a) where
  ui name as1 = termWith name (toAttributes $ Class "a" : as1)



-- we can embed a stylesheet here!
-- options with layoutWith
layout :: UI arg result => arg -> result
layout =
  ui "div" [Class "layout", Class "c"]

row :: UI arg result => arg -> result
row =
  ui "div" [Class "r"]

column :: UI arg result => arg -> result
column =
  ui "div" [Class "c"]

el :: UI arg result => arg -> result
el =
  ui "div" []



-- TODO I feel like this going to come back to bite me
padding :: Int -> Att
padding n = Style "padding" (Pxv n)

spacing :: Int -> Att
spacing n = Style "gap" (Pxv n)

-- TODO make better
background :: Text -> Att
background t = Style "background" (Color t)

height :: Length -> Att
height (Px n) = Style "height" (Pxv n)
height Fill = Class "hf"
height Shrink = Class "hc"

width :: Length -> Att
width (Px n) = Style "width" (Pxv n)
width Fill = Class "wf"
width Shrink = Class "wc"


data Length
  = Fill
  | Shrink
  | Px Int




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
    toValue (Pxv n) = cs (show n) <> "px"
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



-- | Embed built javascript into file via Data.FileEmbed. Must be recompiled via node to work
css :: ByteString
css = $(embedFile "static/ui.css")

stylesheet :: Html ()
stylesheet =
  style_ [rel_ "stylesheet"] css





