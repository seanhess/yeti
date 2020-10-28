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

import Wookie.Runtime (Page(Page), Response(..), runAction, command, Params(encode, decode), Page, PageAction)
import Wookie.Router (parsePath)

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS $ h


-- TODO back button doesn't work: history.onpopstate? Just call it again with the current url. The url is updating
-- TODO Example: username / password validation
-- TODO Example: tab navigation
-- TODO Example: React client-side component (date picker? Not sure what it would be)

-- TODO VDOM: See below, on HTML-REACT-PARSER. Render HTML, parse client-side, convert to a react component
-- TODO better serialization of actions: use `replace` from html-react-parser


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




-- This embeds the javascript into the page
-- How can I tell if they already have it? By the url?
-- Accept encoding!
-- If they ask for Html, give them the whole thing
-- if they ask for Vdom, just give them the one part
reply :: Html () -> ActionM ()
reply h = do
  -- Accept-Encoding: gzip
  -- Accept: application/json
  ha <- header "Accept"

  case ha of
    Just "application/vdom" -> lucid h
    _ -> renderWhole h

  where
    renderWhole :: Html () -> ActionM ()
    renderWhole h' = do
      lucid $ html_ $ do
        head_ $ do
          script_ [type_ "text/javascript", src_ "/js/main.js"] ("test()" :: Text)
        body_ $ do
          h1_ "App"
          div_ [id_ "content"] h'


setPageUrl :: Text -> ActionM ()
setPageUrl u =
  setHeader "X-Page-Url" $ cs u

-- TODO update url via header
-- TODO handle empty body -> load only


-- renderQuery :: Params -> Text
-- renderQuery ps =
--   cs $ renderSimpleQuery True $ fmap toStrings $ Map.toList ps
--   where toStrings (a, b) = (cs a, cs b)


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
    page "/app/about"   About.page

    get "/:name" $ do
      name <- param "name"
      html $ mconcat ["Hello: ", name]


-- give it a route and a page


handlePage 
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ActionM ()
handlePage path pg = do

  p <- (Text.intercalate "&" . fmap (cs. fst)) <$> Scotty.params

  (ps :: params) <- decode p & \case
          Nothing -> fail $ "Could not decode params: " <> cs p
          Just a -> pure a

  cmd <- command =<< body

  Response h s <- runAction pg ps cmd

  setPageUrl (cs path <> "?" <> s)
  reply h


page
  :: forall params model action. (PageAction action, Params params)
  => String -> Page params model action ActionM -> ScottyM ()
page path pg = matchAny (literal path) $ handlePage path pg



delay :: Int -> Application -> Application
delay d application req respond = do
  threadDelay (d * 1000)
  application req respond



-- app :: ActionM ()
-- app = do
--   m    <- body
--   path <- param "path" :: ActionM Text

--   -- what if we don't find a page
--   -- liftIO $ print (path, parsePath path)
--   let Just go = App.resolve path
--   Response view <- liftIO $ go m

--   js <- liftIO $ Text.readFile "js/main.js"

--   lucid $ html_ $ do
--     head_ $ do
--       script_ [type_ "text/javascript"] js
--     body_ view




-- appRoutePattern :: Text -> Request -> Maybe [Param]
-- appRoutePattern prefix req = do
--   let totalPath = cs $ rawPathInfo req
--   appPath <- Text.stripPrefix prefix totalPath
--   pure [("path", cs appPath)]
