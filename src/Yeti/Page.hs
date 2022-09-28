module Yeti.Page where

import Yeti.Prelude
import Yeti.Encode (Encoded(..), Encoding(Model, Action))
import Yeti.Params (QueryText)
import Data.Aeson
import Lucid (Html)






data Page params model action m = Page
  { params :: Params params model
  , load   :: Load params model m
  , update :: Update action model m
  , view   :: View          model
  }


type Load   params model m = Maybe params -> m model
type Params params model   = model -> params
type Update action model m = action -> model -> m model
type View          model   = model -> Html ()

type StaticPage m = Page () () () m
type SimplePage model action m = Page () model action m


-- a page without params
simplePage
  :: forall action model m. Applicative m
  => m model
  -> Update action model m
  -> View model
  -> Page () model action m
simplePage int up vw = Page (const ()) (const int) up vw


staticPage :: Applicative m => Html () -> Page () () () m
staticPage view = Page (const ()) (\_ -> pure ()) (\_ _ -> pure ()) (const view)


-- TODO better encoding
-- TODO generalized!
class (Show page, ToJSON page, FromJSON page) => RoutePage page where
  routePage :: [Text] -> Maybe page


type PageHandler page m = page -> Maybe (Encoded 'Model) -> QueryText -> [Encoded 'Action] -> m Response



-- This needs to be encoded already
data Response = Response
  { resModel :: Encoded 'Model
  , resParams :: QueryText
  , resView :: Html ()
  } deriving (Show, Generic)