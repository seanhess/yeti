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

type AttMod = Attributes -> Attributes

newtype Class = Class { fromClass :: Text }
  deriving newtype (IsString)
  deriving (Show, Eq)


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
data Script
  = Url Text
  | Code Text

newtype VDOM = VDOM { fromVDOM :: [Content] }
  deriving newtype (Generic, ToJSON, FromJSON)

instance Show VDOM where
  show (VDOM cts) = unlines $ fmap show cts

vdom :: View a () -> VDOM
vdom = VDOM . viewContents



newtype View a x = View
  { runView :: State [Content] x
  } deriving newtype (Functor, Applicative, Monad, MonadState [Content])

instance Show (View a x) where
  show u = unlines $ fmap show (viewContents u)

instance IsString (View Content ()) where
  fromString s = do
    modify $ \cts -> cts <> [ Text (cs s)]

viewContents :: View a x -> [Content]
viewContents (View wts) = execState wts []

addContent :: Content -> View a ()
addContent c = modify $ \cts -> cts <> [ c ]




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
