module Yeti.UI.Element where


import Yeti.Prelude
import Yeti.Events (onClick, onInput, onSelect)
import Yeti.Encode (LiveAction, Input)
import Yeti.UI.CSS
import Yeti.View





row :: TagMod a -> View Content () -> View Content ()
-- row f = el (flex Row . flex () . f)
row f = el (flexRow . f)

row_ :: View Content () -> View Content ()
row_ = row id

col :: TagMod a -> View Content () -> View Content ()
-- col f = el (flex Col . flex () . f)
col f = el (flexCol . f)

col_ :: View Content () -> View Content ()
col_ = col id

space :: View Content ()
space = el grow $ pure ()

button :: LiveAction action => action -> TagMod a -> View Content () -> View Content ()
button act f cnt =
  tag "button" (f . onClick act . att "type" "button" . reset) cnt
  -- include the reset incrementally with UI
  where reset = border 0 . pointer


inputText :: LiveAction action => (Text -> action) -> Text -> TagMod a -> View FieldInput ()
inputText act val f = tag "input" (f . onInput act . att "value" val) none


inputSearch :: LiveAction action => (Text -> action) -> Text -> TagMod a -> View FieldInput ()
inputSearch act val f = inputText act val (f . att "role" "search")


hlink :: AttValue -> View a () -> View b ()
hlink href = tag "a" (att "href" href)

-- Automatically wraps it in a label
-- you can pic how it is laid out

data Label
  = LabelAbove
  | LabelBelow
  | LabelRight
  | LabelLeft

data FieldInput


field :: TagMod a -> Label -> Text -> View FieldInput () -> View Content ()
field f LabelAbove l (View ct) =
  tag "label" (flexCol . f) $ do
    text_ l
    View ct

field f LabelBelow l (View ct) =
  tag "label" (flexCol . f) $ do
    View ct
    text_ l

field f LabelRight l (View ct) =
  tag "label" (flexRow . f) $ do
    View ct
    text_ l

field f LabelLeft l (View ct) =
  tag "label" (flexRow . f) $ do
    text_ l
    View ct

  
dropdown :: (LiveAction action, Input val) => (val -> action) -> TagMod a -> (val -> Text) -> (val -> View Content ()) -> [val] -> View FieldInput ()
dropdown act f toVal opt vals =
  tag "select" (f . onSelect act) $
    mapM_ option vals
  where
    option v = tag "option" (att "value" (toVal v)) (opt v)