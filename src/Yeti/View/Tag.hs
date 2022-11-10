module Yeti.View.Tag where

import Yeti.Prelude
import Yeti.View.Types
import qualified Data.Map as Map
import qualified Data.Text as Text



-- | Add a class attribute. If it exists, combine with spaces
cls :: [Class] -> TagMod
cls cx t =
  t { classes = t.classes <> cx }

-- | Set an attribute, replacing existing value
att :: Name -> AttValue -> TagMod
att n v t = t { attributes = Map.insert n v t.attributes }





tag :: Text -> TagMod -> View a () -> View b ()
tag nm f ctu = addContent $
  Node $ f $ Tag nm [] [] (viewContents ctu)


-- type TagF = TagMod -> View Content () -> View Content ()
-- (#) :: TagF -> View Content () -> View Content ()
-- f # ct = f id ct
-- infixr 8 #

-- | A generic node with style, attributes, and content 
el :: TagMod -> View Content () -> View Content ()
el = tag "div"

el_ :: View Content () -> View Content ()
el_ = tag "div" id

-- | A styled inline text node
text :: TagMod -> Text -> View Content ()
text f ct = tag "span" f (fromText ct)

text_ :: Text -> View Content ()
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










meta :: TagMod -> View a ()
meta f = tag "meta" f (fromText "")

title_ = tag "title" id

head_ = tag "head" id

html_ = tag "html" id

body_ = tag "body" id


