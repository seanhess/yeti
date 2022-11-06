{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.View.Types where

import Yeti.Prelude
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.Monad.Writer.Lazy (Writer, execWriter, tell, MonadWriter)
import Control.Monad.State.Strict (State, withState, execState, modify, put, get, MonadState)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.String (IsString(..))
import qualified Data.Text.Lazy as Lazy
import Debug.Trace (traceM)


type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

-- Allows you to only specify attributes of a given type
-- TODO but this won't work very well, because the same attribute could be used for two different things
type Att a = Attributes -> Attributes

newtype Class = Class { fromClass :: Text }
  deriving newtype (IsString)
  deriving (Show, Eq)



-- | Add a class attribute. If it exists, combine with spaces
cls :: [Class] -> Attributes -> Attributes
cls cx = Map.insertWith combine "class" (classes cx)
  where
    combine new old = new <> " " <> old
    classes = Text.intercalate " " . fmap fromClass

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> Att a
att = Map.insert



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
  , attributes :: Attributes
  , children :: [Content]
  } deriving (Show)
-- encode as 3 element array

instance ToJSON Tag where
  toJSON (Tag n a c) =
    toJSON (n, a, c)

instance FromJSON Tag where
  parseJSON val = do
    (n, a, c) <- parseJSON val
    pure $ Tag n a c

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

instance FromJSON Content where
  parseJSON v =
    Node <$> parseJSON v <|> Text <$> parseJSON v

data Document
data Body

newtype VDOM = VDOM { fromVDOM :: [Content] }
  deriving newtype (Generic, ToJSON, FromJSON)

instance Show VDOM where
  show (VDOM cts) = unlines $ fmap show cts

vdom :: View a () -> VDOM
vdom = VDOM . execViewContent



test :: View a ()
test = do
  col & att' "key" "value" $ do

    row & gap8 . pad4 $ do
      fromText "one"
      text & gap4 $ "two"

    row $ text "three"

  where
    row :: View a () -> View b ()
    row = tag "div" id

    col :: View a () -> View b ()
    col = tag "div" id

    gap8 = cls' [Class "gap-8"]
    pad4 = cls' [Class "pad-4"]
    gap4 = cls' [Class "gap-4"]

    text :: Text -> View b ()
    text = tag "span" id . fromText

    -- well, we don't want to add it to the *content*
    att' :: Name -> AttValue -> (a -> View b ()) -> a -> View b ()
    att' n v addContent =
      mapTag (modifyAtt (Map.insert n v)) . addContent

    cls' :: [Class] -> (a -> View b ()) -> a -> View b ()
    cls' cx addContent =
      mapTag (modifyAtt (cls cx)) . addContent

    modifyAtt :: (Attributes -> Attributes) -> Tag -> Tag
    modifyAtt f t = t { attributes = f t.attributes }

    -- applies a transformation only to tag nodes
    mapTag :: (Tag -> Tag) -> View a () -> View a ()
    mapTag f = do
      mapViewContents (map (mapTagContent f))

    mapViewContents :: ([Content] -> [Content]) -> View a () -> View a ()
    mapViewContents f (View st) = modify $ f . execState st

    -- needs to map each of them
    mapTagContent :: (Tag -> Tag) -> Content -> Content
    mapTagContent f (Node t) = Node $ f t
    mapTagContent _ (Text t) = Text t



newtype View a x = View
  { runView :: State [Content] x
  } deriving newtype (Functor, Applicative, Monad, MonadState [Content])


instance Show (View a x) where
  show u = unlines $ fmap show (execViewContent u)

instance IsString (View Content ()) where
  fromString s = do
    modify $ \cts -> cts <> [ Text (cs s)]


execViewContent :: View a x -> [Content]
execViewContent (View wts) = execState wts []

tag :: Text -> Att a -> View a () -> View b ()
tag nm f ctu = modify $ \cts ->
  cts <> [ Node $ Tag nm (f []) (execViewContent ctu) ]

-- | A generic node with style, attributes, and content 
el :: Att a -> View Content () -> View Content ()
el = tag "div"

-- | A styled inline text node
-- text :: Text -> View Content ()
-- text ct = tag "span" id (fromText ct)

-- | Convert from text directly to view. You should not have to use this. Use `text` instead
fromText :: Text -> View a ()
fromText t = modify $ \cts ->
  cts <> [ Text t ]









-- Render HTML

toHtmlText :: View Document () -> Text
toHtmlText = htmlDocument

toHtmlLazyText :: View Document () -> Lazy.Text
toHtmlLazyText = cs . htmlDocument

htmlDocument :: View Document () -> Text
htmlDocument u = 
  let ts = execViewContent u
  in case ts of
    [Node d] -> mconcat $ htmlTag noIndent 0 d
    cts -> error "Should not be possible to create document with multiple tags"

showView :: View a () -> Text
showView v = Text.unlines $ mconcat $ map showContent $ execViewContent v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag indent 0 t
showContent (Text t) = [t]


type Indent = Int

htmlTag :: (Indent -> [Text] -> [Text]) -> Indent -> Tag -> [Text]
htmlTag indent' i (Tag name atts cnt) =
  case cnt of

    [] -> [ open <> htmlAtts atts <> "/>" ]

    [Text t] ->
      [ open <> htmlAtts atts <> ">" <> t <> close ]

    _  -> mconcat
      [ [ open <> htmlAtts atts <> ">" ]
      , indent' (i+1) $ htmlChildren cnt
      , [ close ]
      ]

  where
    open = "<" <> name
    close = "</" <> name <> ">"

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
      where htmlAtt (k, v) =
              k <> "=" <> "'" <> v <> "'"

indent :: Indent -> [Text] -> [Text]
indent i' = fmap ind
  where
    ind :: Text -> Text
    ind t = Text.replicate (2*i') " " <> t

noIndent :: Indent -> [Text] -> [Text]
noIndent _ ts = ts
