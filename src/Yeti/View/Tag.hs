module Yeti.View.Tag where

import Yeti.Prelude
import Yeti.View.Types
    ( TagMod,
      View,
      Content(..),
      addContent,
      viewContents,
      AttValue,
      Attribute,
      Class,
      Name,
      Script(..),
      Tag(Tag, classes, attributes),
      viewClasses,
      addClasses,
      classList )
import qualified Data.Map as Map
import qualified Data.Text as Text



-- | Add a class attribute. If it exists, combine with spaces
-- this can be MULTIPLE classes, yay
cls :: [Class] -> TagMod Class
cls cx t =
  t { classes = cx : t.classes }

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> TagMod Attribute
att n v t = t { attributes = Map.insert n v t.attributes }





tag :: Text -> TagMod a -> View a () -> View b ()
tag nm f ctu = do
  let t = f $ Tag nm [] [] (viewContents ctu)
  addContent $ Node t
  addClasses $ mconcat $ t.classes
  addClasses $ classList $ viewClasses ctu


-- type TagF = TagMod -> View Content () -> View Content ()
-- (#) :: TagF -> View Content () -> View Content ()
-- f # ct = f id ct
-- infixr 8 #

-- | A generic node with style, attributes, and content 
el :: TagMod a -> View Content () -> View Content ()
el = tag "div"

el_ :: View Content () -> View Content ()
el_ = tag "div" id

-- | A styled inline text node
text :: TagMod a -> Text -> View a ()
text f ct = tag "span" f (fromText ct)

text_ :: Text -> View a ()
text_ ct = tag "span" id (fromText ct)


-- | Convert from text directly to view. You should not have to use this. Use `text` instead
fromText :: Text -> View a ()
fromText t = addContent $ Text t

script :: Script -> View b ()
script (Url src) = tag "script" (att "type" "text/javascript" . att "src" src) none
script (Code code) = tag "script" (att "type" "text/javascript") $ fromText code

stylesheet :: Text -> View b ()
stylesheet href = tag "link" (att "rel" "stylesheet" . att "href" href) none

style :: Text -> View b ()
style ct = tag "style" (att "type" "text/css") $ fromText ct

none :: View Content ()
none = ""










meta :: TagMod a -> View a ()
meta f = tag "meta" f (fromText "")

title_ = tag "title" id

head_ = tag "head" id

html_ = tag "html" id

body_ = tag "body" id

