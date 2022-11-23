{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.View.Types where

import Yeti.Prelude
import Data.Aeson (ToJSON(..), FromJSON(..), Value(String))
import Control.Monad.Writer.Lazy (Writer, execWriter, tell, MonadWriter)
import Control.Monad.State.Strict (State, withState, execState, modify, put, get, MonadState, gets)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.String (IsString(..))
import qualified Data.Text.Lazy as Lazy
import Debug.Trace (traceM)


type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

type TagMod = Tag -> Tag



data Class = Class
  { className :: ClassName
  , classProperties :: ClassProps
  } deriving (Show)

type ClassProps = Map Text ClassValue


type ClassName = String
data ClassValue = ClassValue
  { value :: String
  , units :: Units
  } deriving (Show)

instance IsString ClassValue where
  fromString s = ClassValue s None

instance ToJSON ClassValue where
  toJSON = String . renderClassValue


data Units
  = None
  | Px
  | Rem

instance Show Units where
  show None = ""
  show Px = "px"
  show Rem = "rem"


-- -- TODO make sure purging works!
-- (|:) :: Tailwind.Prefix -> (Opt a -> Opt a) -> Att a
-- (|:) p f o = mapFirstClass (Tailwind.addPrefix p) $ f o
--   where
--     mapFirstClass f_ (Opt as (cs:css)) = Opt as (f_ cs:css)
--     mapFirstClass f_ o' = o'
-- infixr 9 |:


attribute :: Name -> AttValue -> Attribute
attribute n v = (n, v)

data Tag = Tag
  { name :: Name
  , classes :: [Class]
  , attributes :: Attributes
  , children :: [Content]
  } deriving (Show)

instance ToJSON Tag where
  -- encode as 3 element array, so the footprint is smaller
  toJSON t =
    -- then it needs to serialize all the classes
    toJSON (t.name, tagAttributes t, t.children)

tagAttributes :: Tag -> Attributes
tagAttributes t =
  addClass t.classes t.attributes

  where
    addClass [] atts = atts
    addClass cx atts = Map.insert "class" (classAttValue cx) atts

    classAttValue cx =
      Text.intercalate " " $ map (cs . (.className)) cx


data Content
  = Node Tag
  | Text Text
  deriving (Generic)

instance Show Content where
  show (Node t) = cs $ Text.intercalate "\n" $ htmlTag indent 0 t
  show (Text t) = cs t

instance ToJSON Content where
  toJSON (Node t) = toJSON t
  toJSON (Text t) = toJSON t

-- instance FromJSON Content where
--   parseJSON v =
--     Node <$> parseJSON v <|> Text <$> parseJSON v

data Document
data Body
data Script
  = Url Text
  | Code Text

newtype VDOM = VDOM { fromVDOM :: [Content] }
  deriving newtype (Generic, ToJSON)

instance Show VDOM where
  show (VDOM cts) = unlines $ fmap show cts

vdom :: View a () -> VDOM
vdom = VDOM . viewContents

-- viewClasses :: View a x -> 
nestedClasses :: View a () -> Map ClassName ClassProps
nestedClasses (View st) = do
  (.classes) $ execState st (ViewState [] [])



renderCSS :: Map ClassName ClassProps -> [Text]
renderCSS m = map renderClass $ toClasses m
  where
    toClasses = map toClass . Map.toList
    toClass (n, p) = Class n p

    renderClass :: Class -> Text
    renderClass (Class n p) =
      "." <> (cs n) <> " " <> "{" <> (Text.intercalate "; " $ map renderProp $ Map.toList p) <> "}"

    renderProp :: (Text, ClassValue) -> Text
    renderProp (p, cv) = p <> ":" <> renderClassValue cv

renderClassValue :: ClassValue -> Text
renderClassValue (ClassValue v u) = cs $ v <> show u


newtype View a x = View
  { runView :: State ViewState x
  } deriving newtype (Functor, Applicative, Monad, MonadState ViewState)

data ViewState = ViewState
  { contents :: [Content]
  , classes :: Map ClassName ClassProps
  } deriving (Show)

instance Show (View a x) where
  show u = unlines $ fmap show (viewContents u)

instance IsString (View Content ()) where
  fromString s = do
    addContent $ Text (cs s)

viewContents :: View a x -> [Content]
viewContents (View wts) = (.contents) $ execState wts (ViewState [] [])

addContent :: Content -> View a ()
addContent c = modify $ \vs -> vs { contents = vs.contents <> [ c ]}

-- classAttribute :: [Class] -> Attribute
-- classAttribute cls = ("class", Text.intercalate " " $ map (cs . (.className)) cls)

-- Render HTML

toHtmlText :: View Document () -> Text
toHtmlText = htmlDocument

toHtmlLazyText :: View Document () -> Lazy.Text
toHtmlLazyText = cs . htmlDocument

htmlDocument :: View Document () -> Text
htmlDocument u = 
  let ts = viewContents u

  in case ts of
    [Node d] -> mconcat $ htmlTag noIndent 0 d
    cts -> error "Should not be possible to create document with multiple tags. Use document function."

showView :: View a () -> Text
showView v = Text.unlines $ mconcat $ map showContent $ viewContents v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag indent 0 t
showContent (Text t) = [t]


type Indent = Int

htmlTag :: (Indent -> [Text] -> [Text]) -> Indent -> Tag -> [Text]
htmlTag indent' i tag =
  case tag.children of

    [] -> [ open <> htmlAtts (tagAttributes tag) <> "/>" ]

    [Text t] ->
      [ open <> htmlAtts (tagAttributes tag) <> ">" <> t <> close ]

    _  -> mconcat
      [ [ open <> htmlAtts (tagAttributes tag) <> ">" ]
      , indent' (i+1) $ htmlChildren tag.children
      , [ close ]
      ]

  where
    open = "<" <> tag.name
    close = "</" <> tag.name <> ">"

    htmlContent :: Content -> [Text]
    htmlContent (Node t) = htmlTag indent' i t
    htmlContent (Text t) = [t]

    htmlChildren :: [Content] -> [Text]
    htmlChildren cts = mconcat $
      fmap htmlContent cts

    htmlAtts :: Attributes -> Text
    htmlAtts [] = ""
    htmlAtts as = " " <> 
      Text.intercalate " " (map htmlAtt $ Map.toList as)
      where
        htmlAtt (k, v) =
          k <> "=" <> "'" <> v <> "'"

indent :: Indent -> [Text] -> [Text]
indent i' = fmap ind
  where
    ind :: Text -> Text
    ind t = Text.replicate (2*i') " " <> t

noIndent :: Indent -> [Text] -> [Text]
noIndent _ ts = ts
