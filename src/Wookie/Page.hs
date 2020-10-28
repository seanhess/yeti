{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Wookie.Page
 ( Page(Page)
 , PageAction(..)
 ) where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Control.Monad.State.Lazy (StateT)
import Lucid (Html)
import Text.Read (readMaybe)





data Page params model action m = Page
  { params :: model -> params
  , load   :: params -> m model
  , update :: action -> StateT model m ()
  , view   :: model -> Html ()
  }





class PageAction a where
  showAction :: a -> Text
  readAction :: Text -> Maybe a

  default showAction :: Show a => a -> Text
  showAction = cs . show

  default readAction :: Read a => Text -> Maybe a
  readAction = readMaybe . cs

instance PageAction () where
  showAction _ = ""
  readAction _ = Just ()







