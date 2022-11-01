{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Yeti.View.Types where

import Yeti.Prelude
import Data.Aeson (ToJSON(..), FromJSON(..))
import Control.Monad.Writer.Lazy (Writer, execWriter, tell, MonadWriter)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.String (IsString(..))


type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

type Att a = Attributes -> Attributes

newtype Class = Class { fromClass :: Text }
  deriving newtype (IsString)
  deriving (Show, Eq)



-- | Add a class attribute. If it exists, combine with spaces
cls :: [Class] -> Attributes -> Attributes
cls cx as = Map.insertWith combine "class" (classes cx) as
  where
    combine new old = new <> " " <> old
    classes = Text.intercalate " " . fmap (fromClass)

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> Att a
att k v = Map.insert k v



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
  show (Node t) = cs $ Text.intercalate "\n" $ htmlTag 0 t
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


newtype View a x = View
  { runView :: Writer [Content] x
  } deriving newtype (Functor, Applicative, Monad, MonadWriter [Content])

instance Show (View a x) where
  show u = unlines $ fmap show (execViewContent u)

instance IsString (View Content ()) where
  fromString s = tell [ Text (cs s) ]


execViewContent :: View a x -> [Content]
execViewContent (View wts) = execWriter wts

tag :: Text -> Attributes -> View a () -> View Content ()
tag nm as ctu = tell
  [ Node $ Tag nm as (execViewContent ctu) ]

-- | A generic node with style, attributes, and content 
el :: Att a -> View a () -> View Content ()
el f ct = tag "div" (f []) ct

-- | A styled inline text node
txt :: Att a -> Text -> View Content ()
txt f ct = tag "span" (f []) (fromText ct)

-- | Convert from text directly to view. You should not have to use this. Use `text` instead
fromText :: Text -> View a ()
fromText t = tell
  [ Text t ]

type DocumentTitle = Text

document :: DocumentTitle -> View Content () -> View Document ()
document title body = View $ runView $
  html' $ do
    head' $ do
      title' $ fromText title
      meta (charset "UTF-8")
      meta (httpEquiv "Content-Type" . content "text/html" . charset "UTF-8")
      meta (name "viewport" . content "width=device-width, initial-scale=1.0")
    tag "body" [] body
  where
    meta f = tag "meta" (f []) (fromText "")
    title' = tag "title" []
    head' = tag "head" []
    html' = tag "html" []
    body' = tag "body" []
    charset = att "charset"
    httpEquiv = att "httpEquiv"
    content = att "content"
    name = att "name"










-- Render HTML

toHtmlText :: View Document () -> Text
toHtmlText = htmlDocument

htmlDocument :: View Document () -> Text
htmlDocument u = 
  let ts = execViewContent u
  in case ts of
    [Node d] -> Text.unlines $ htmlTag 0 d
    cts -> error $ "Should not be possible to create document with multiple tags"

showView :: View a () -> Text
showView v = Text.unlines $ mconcat $ fmap showContent $ execViewContent v

showContent :: Content -> [Text]
showContent (Node t) = htmlTag 0 t
showContent (Text t) = [t]


type Indent = Int

htmlTag :: Indent -> Tag -> [Text]
htmlTag i (Tag name atts cnt) =
  case cnt of

    [] -> [ open <> htmlAtts atts <> "/>" ]

    [Text t] ->
      [ open <> htmlAtts atts <> ">" <> t <> close ]

    _  -> mconcat
      [ [ open <> htmlAtts atts <> ">" ]
      , indent (i+1) $ htmlChildren cnt
      , [ close ]
      ]

  where
    open = "<" <> name
    close = "<" <> name <> ">"

    htmlContent :: Content -> [Text]
    htmlContent (Node t) = htmlTag i t
    htmlContent (Text t) = [t]

    htmlChildren :: [Content] -> [Text]
    htmlChildren cts = mconcat $
      fmap htmlContent cts

    htmlAtts :: Attributes -> Text
    htmlAtts [] = ""
    htmlAtts as = " " <> 
      (Text.intercalate " " $ fmap htmlAtt $ Map.toList as)
      where htmlAtt ((k, v)) =
              k <> "=" <> "'" <> v <> "'"

    indent :: Indent -> [Text] -> [Text]
    indent i' = fmap ind
      where
        ind :: Text -> Text
        ind t = Text.replicate (2*i') " " <> t
