{-# LANGUAGE OverloadedLists #-}
module Yeti.VDOM where

import Yeti.Prelude
import Control.Monad.Writer.Lazy (Writer, execWriter, tell, MonadWriter)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Data.Aeson


test :: IO ()
test = do
  putStrLn "TEST"
  print doc
  print content
  let out = encode $ toVDOM $ content
  putStrLn $ cs out
  print $ (decode out :: Maybe VDOM)
  where
    doc = document "my title" content

    content = do
      tag "div" [attribute "key" "value"] $ text "hello world"
      tag "section" [] $ do
        tag "div" [] $ text "one"

        -- currently successive attributes replace old ones
        -- that kind of makes sense

        tag "div" [attribute "class" "check", attribute "class" "woot"] $ text "two"
        tag "div" [] $ text "three"

-- data Attribute = Attribute
--   { name :: Text
--   , value :: Text
--   } deriving (Generic, ToJSON, FromJSON)

type Name = Text
type AttValue = Text

type Attribute = (Name, AttValue)
type Attributes = Map Name AttValue

attribute :: Name -> AttValue -> Attribute
attribute n v = (n, v)

data Tag = Tag
  { name :: Name
  , attributes :: Attributes
  , children :: [Content]
  } deriving (Generic, ToJSON, FromJSON)

data Content
  = Node Tag
  | Text Text
  deriving (Generic)

instance ToJSON Content where
  toJSON (Node t) = toJSON t
  toJSON (Text t) = toJSON t

instance FromJSON Content where
  parseJSON v =
    Node <$> parseJSON v <|> Text <$> parseJSON v





instance Show Content where
  show (Node t) = cs $ Text.unlines $ htmlTag 0 t
  show (Text t) = cs t

-- Document is something else, for sure
data Document
data Body

-- you're creating a View?
newtype VDOM = VDOM [Content]
  deriving newtype (Generic, ToJSON, FromJSON)

instance Show VDOM where
  show (VDOM cts) = unlines $ fmap show cts

toVDOM :: UI a () -> VDOM
toVDOM = VDOM . execUIContent


newtype UI a x = UI
  { runUI :: Writer [Content] x
  } deriving newtype (Functor, Applicative, Monad, MonadWriter [Content])

instance Show (UI a x) where
  show u = unlines $ fmap show (execUIContent u)

execUIContent :: UI a x -> [Content]
execUIContent (UI wts) = execWriter wts

tag :: Text -> Attributes -> UI a () -> UI b ()
tag nm as ctu = tell
  [ Node $ Tag nm as (execUIContent ctu) ]

text :: Text -> UI a ()
text t = tell
  [ Text t ]

-- htmlText :: Text -> UI Text ()
-- htmlText t = 

document :: Text -> UI Body () -> UI Document ()
document title body =
  tag "html" [] $ do
    tag "head" [] $ do
      tag "title" [] $ text title
    tag "body" [] body


-- we ignore all the tags
-- you can't create one of these without using our function
htmlDocument :: UI Document () -> Text
htmlDocument u = 
  let ts = execUIContent u
  in case ts of
    [Node d] -> Text.unlines $ htmlTag 0 d
    cts -> error $ "Should not be possible to create document with multiple tags: \n" <> show cts




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
indent i = fmap ind1
  where
    ind1 :: Text -> Text
    ind1 t = Text.replicate (2*i) " " <> t
