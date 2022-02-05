{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Juniper.Page
 ( Page(Page)
 , PageAction(..)
 , simplePage
 ) where

import Data.String.Conversions (cs)
import Data.Text as Text (Text)
import Lucid (Html)
import Text.Read (readMaybe)


import Data.Map (Map)



data Page params model action m = Page
  { params :: Params params model
  , reload :: Reload params model m
  , update :: Update action model m
  , view   :: View          model
  }

type Reload params model m = Maybe params -> m model
type Params params model   = model -> params
type Load          model m = m model
type Update action model m = action -> model -> m model
type View          model   = model -> Html ()


simplePage
  :: forall action model m. Applicative m
  => Load model m
  -> Update action model m
  -> View model
  -> Page model model action m
simplePage it up vw = Page id initLoad up vw
  where
    initLoad :: Reload model model m
    initLoad (Just m) = pure m
    initLoad Nothing = it

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


