{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.About where


import Data.Text (Text)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5

import Wookie.Runtime (Page(Page))
import Control.Monad.State.Lazy (StateT)
import Control.Monad.IO.Class (MonadIO)


type Model = ()
type Params = ()
type Action = ()

page :: MonadIO m => Page Params Model Action m
page = Page params load update view


params :: Model -> Params
params = id

load :: Monad m => Params -> m Model
load _ = pure ()


-- does it have to be IO?
-- no.... it should be any MonadIO
update :: Monad m => Action -> StateT Model m ()
update _ = pure ()





view :: () -> Html ()
view m = div_ $ do
  h1_ "About"
  p_ $ a_ [href_ "/app/counter?count=77"] "Counter 77"



