{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Juniper.Page
 ( Page(Page)
 , PageAction(..)
 , stripArgs
 ) where

import Data.String.Conversions (cs)
import Data.Text as Text (Text)
import Data.List as List
import Lucid (Html)
import Text.Read (readMaybe)


import Data.Map (Map)



data Page params model action m = Page
  { params :: model -> params
  , load   :: Maybe params -> m model
  , update :: action -> model -> m model
  , view   :: model -> Html ()
  }




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

