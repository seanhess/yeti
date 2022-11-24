{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.View.Types where

import Yeti.Prelude
import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), ToJSONKey(..), ToJSONKeyFunction(..))
import Control.Monad.Writer.Lazy (Writer, execWriter, tell, MonadWriter)
import Control.Monad.State.Strict (State, withState, execState, modify, put, get, MonadState, gets)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.String (IsString(..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as Text
import Debug.Trace (traceM)
import Network.Socket (Family(Pseudo_AF_HDRCMPLT))


type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

type TagMod a = Tag -> Tag



data Class = Class
  { className' :: ClassName
  , classProperties :: ClassProps
  } deriving (Show)

classFromPair :: (ClassName, ClassProps) -> Class
classFromPair (n,p) = Class n p


type ClassProps = Map Text Style

className :: Class -> Text
className c = classNameText c.className'

classNameSelector :: ClassName -> Text
classNameSelector (ClassName NoPsd n) = n
classNameSelector (ClassName p n) =
    pseudoText p <> "\\:" <> n <> ":" <> pseudoText p

classNameText :: ClassName -> Text
classNameText (ClassName NoPsd n) = n
classNameText (ClassName p n) =
    (pseudoText p) <> ":" <> n

pseudoText :: Pseudo -> Text
pseudoText NoPsd = ""
pseudoText p = Text.toLower $ cs $ show p

data ClassName = ClassName
  { _pseudo :: Pseudo
  , _name :: Text
  } deriving (Show, Eq)

instance IsString ClassName where
  fromString s = ClassName NoPsd (cs s)

instance Ord ClassName where
  compare a b = compare (classNameText a) (classNameText b)

instance ToJSON ClassName where
  toJSON cn = String $ classNameSelector cn

instance ToJSONKey ClassName where
  toJSONKey =
    let ToJSONKeyText tkey tenc = toJSONKey :: ToJSONKeyFunction Text
    in ToJSONKeyText (toKey tkey) (toEnc tenc)
    where
      toKey f c = f (classNameSelector c)
      toEnc f c = f (classNameSelector c)



data Pseudo
  = NoPsd
  | Hover
  deriving (Show, Eq)

data Style = Style
  { units :: Units
  , value :: String
  }

instance Show Style where
  show cv = unitsValue cv.units cv.value

instance IsString Style where
  fromString s = Style None s

instance ToJSON Style where
  toJSON = String . renderStyle


data Units
  = None
  | Px
  | Rem
  | Hex
  | RGB

unitsValue :: Units -> String -> String
unitsValue None s = s
unitsValue Px s = s <> "px"
unitsValue Rem s = s <> "rem"
unitsValue Hex s = "#" <> s
-- it needs to have a string?
-- this might need to get more complicated
unitsValue RGB s = "rgb("<> s <>")"



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
  , classes :: [[Class]]
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
  addClass (mconcat t.classes) t.attributes

  where
    addClass [] atts = atts
    addClass cx atts = Map.insert "class" (classAttValue cx) atts

    classAttValue :: [Class] -> Text
    classAttValue cx =
      Text.intercalate " " $ map (cs . className) cx


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

viewClasses :: View a () -> Map ClassName ClassProps
viewClasses (View st) = do
  (.classStyles) $ execState st (ViewState [] [])

viewContents :: View a x -> [Content]
viewContents (View wts) = (.contents) $ execState wts (ViewState [] [])

classList :: Map ClassName ClassProps -> [Class]
classList m = map classFromPair $ Map.toList m


renderCSS :: Map ClassName ClassProps -> [Text]
renderCSS m = map renderClass $ toClasses m
  where
    toClasses = map toClass . Map.toList
    toClass (n, p) = Class n p

    renderClass :: Class -> Text
    renderClass (Class n p) =
      "." <> (classNameSelector n) <> " " <> "{" <> (Text.intercalate "; " $ map renderProp $ Map.toList p) <> "}"

    renderProp :: (Text, Style) -> Text
    renderProp (p, cv) = p <> ":" <> renderStyle cv

renderStyle :: Style -> Text
renderStyle (Style v u) = cs $ unitsValue v u


newtype View a x = View
  { runView :: State ViewState x
  } deriving newtype (Functor, Applicative, Monad, MonadState ViewState)

data ViewState = ViewState
  { contents :: [Content]
  , classStyles :: Map ClassName ClassProps
  } deriving (Show)

instance Show (View a x) where
  show u = unlines $ fmap show (viewContents u)

instance IsString (View Content ()) where
  fromString s = do
    addContent $ Text (cs s)

addClasses :: [Class] -> View a ()
addClasses clss = do
  modify $ \vs -> vs
    { classStyles = foldr addClsDef vs.classStyles clss
    }
  where
    addClsDef :: Class -> Map ClassName ClassProps -> Map ClassName ClassProps
    addClsDef c = Map.insert c.className' c.classProperties

addContent :: Content -> View a ()
addContent ct = do
  modify $ \vs -> vs
    { contents = vs.contents <> [ ct ]
    }


contentClasses :: Content -> [Class]
contentClasses (Text _) = []
contentClasses (Node t) = mconcat t.classes

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
