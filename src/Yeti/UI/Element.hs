module Yeti.UI.Element where


import Yeti.Prelude
import Yeti.Events (onClick, onInput)
import Yeti.Encode (LiveAction)
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
  where reset = border 0


inputText :: LiveAction action => (Text -> action) -> Text -> TagMod a -> View FieldInput ()
inputText act val f = tag "input" (f . onInput act . att "value" val) none


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

-- this forces you to have a label
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

  

-- <label>
--    <input type="text" name="lastname" />
--    Last Name
-- </label>







