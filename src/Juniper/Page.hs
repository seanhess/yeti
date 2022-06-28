module Juniper.Page
 ( Page(Page)
 , PageAction(..)
 , simplePage
 ) where

import Juniper.Prelude
import Lucid (Html)
import Text.Read (readMaybe)



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


-- a page without params
simplePage
  :: forall action model m. Applicative m
  => m model
  -> Update action model m
  -> View model
  -> Page () model action m
simplePage int up vw = Page (const ()) (const int) up vw

-- like show, but custom, and we know what we support

class PageAction a where
  showAction :: a -> String
  readAction :: String -> Maybe a

  -- TODO using show won't work. They might want to override show for another reason
  default showAction :: Show a => a -> String
  showAction = show

  default readAction :: Read a => String -> Maybe a
  readAction = readMaybe

instance PageAction () where
  showAction _ = "o"
  readAction _ = Just ()


