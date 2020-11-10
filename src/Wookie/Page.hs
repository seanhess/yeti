{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Wookie.Page
 ( Page(Page)
 , PageAction(..)
 , Update
 , stripArgs
 ) where

import Data.String.Conversions (cs)
import Data.Text as Text (Text)
import Data.List as List
import Control.Monad.State.Lazy (StateT)
import Lucid (Html)
import Text.Read (readMaybe)


import Data.Map (Map)



type Update = StateT


data Page params model action m = Page
  { params :: model -> params
  , load   :: Maybe params -> m model
  , update :: action -> Update model m ()
  , view   :: model -> Html ()
  }




-- like show, but custom, and we know what we support

class PageAction a where
  showAction :: a -> String
  readAction :: String -> Maybe a

  default showAction :: Show a => a -> String
  showAction = show

  default readAction :: Read a => String -> Maybe a
  readAction = readMaybe

instance PageAction () where
  showAction _ = "o"
  readAction _ = Just ()




-- class Argument a where
--   anything :: a

-- instance Monoid a => Argument (Arg a) where
--   anything = Arg mempty

-- newtype Arg a = Arg { arg :: a }

-- instance Show (Arg a) where
--   show _ = "asdf"




-- I need one that strips the trailing "_" off the end
-- strip all of them!

stripArgs :: String -> String
stripArgs = List.dropWhileEnd isArg
  where isArg c = c == ' ' || c == '_'

