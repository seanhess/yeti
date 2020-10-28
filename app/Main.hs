{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace (traceM)
import Data.String.Conversions (cs)
import Data.Text as Text (stripPrefix, intercalate, Text)
import Data.Text.IO as Text (readFile)
import qualified Data.Text.Lazy as TL
import Data.Map as Map (fromList, toList, keys)
import Web.Scotty as Scotty
import Network.Wai (Request, pathInfo, rawPathInfo, Application)
import Lucid (renderBS, Html)
import Lucid.Html5 (html_, head_, script_, src_, body_, type_, h1_, id_, div_)
-- import Counter (view, Model(..), load, update, view)
-- import App (resolve)
import qualified Page.Counter as Counter
import qualified Page.About as About
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Function ((&))

import Control.Monad.State.Lazy (StateT, execStateT)
import Text.Read (readMaybe)

import Network.HTTP.Types.URI (renderSimpleQuery)

import Wookie.Runtime (Response(..), runAction, command)
import Wookie.Router (parsePath)
import Wookie.Web (page, lucid, static)


-- TODO back button doesn't work: history.onpopstate? Just call it again with the current url. The url is updating
-- TODO Example: username / password validation
-- TODO Example: tab navigation
-- TODO Example: React client-side component (date picker? Not sure what it would be)

-- TODO VDOM: See below, on HTML-REACT-PARSER. Render HTML, parse client-side, convert to a react component
-- TODO better serialization of actions: use `replace` from html-react-parser

-- TODO update url via header
-- TODO handle empty body -> load only


-- HTML-REACT-PARSER
-- =========================================================
-- https://github.com/remarkablemark/html-react-parser
-- https://www.npmjs.com/package/html-react-parser - let's you swap out certain elements with components, cool.
-- So you should be able to drop in react components and have it work!
-- I can put fancy things in (NOT JAVASCRIPT) and my component can replace them with working coolness
-- XSS - i need to escape the  rendered input on the server
-- Lucid already escapes things! So <script> with give you: &lt;script:gt;. It even escapes quotes. Sick. How does it still work??
-- Just test an XSS attack and see if you can get it to work



-- VDOM =============
-- Virtual Dom Javascript library - looks unmaintained
-- React - why not communicate directly to react? we could probably create view code. Makes embedding other react components easy. Makes people feel happy
-- Elm - probably impossible without reproducing the views

-- <MyButton color="blue" shadowSize={2}>
  -- Click Me
-- </MyButton>
-- React.createElement(
  -- MyButton,
  -- {color: 'blue', shadowSize: 2},
  -- 'Click Me'
-- )
-- <div className="sidebar" />
-- React.createElement(
  -- 'div',
  -- {className: 'sidebar'}
-- )



main :: IO ()
main = do

  -- load embedded js

  scotty 3000 $ do
    middleware (delay 100)

    get "/js/main.js" $ do
      setHeader "Content-Type" "text/javascript"
      file "js/main.js"

    -- pages! This feels way more magical than it should, I think :(
    page "/app/counter" Counter.page

    -- if you use "lucid" it doesn't work
    static "/app/about" About.view

    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]








delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond



