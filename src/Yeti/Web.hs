{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Yeti.Web
  ( Render(..)
  , respond
  , static
  , def
  , page
  , pageUrl
  , simpleDocument
  , lucid
  , queryToText
  , params
  , input'
  ) where

import Yeti.Prelude
import Yeti.Runtime as Runtime (Response(..), Page, PageHandler)
import Yeti.Encode (LiveModel, LiveAction, encodeModel, fromEncoded, Encoded(..), Encoding(Model))
import qualified Yeti.Encode as Encode
import qualified Yeti.Runtime as Runtime
import Yeti.Params as Params (ToParams(..), urlEncode, urlDecode)
import Yeti.JS as JS
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Base64 (encodeBase64, decodeBase64)
import Data.Text as Text (intercalate, dropWhile)
import Data.List as List (lookup)
import Web.Scotty (RoutePattern)
import Web.Scotty.Trans (ActionT, ScottyT, ScottyError)
import qualified Web.Scotty.Trans as Scotty
import Lucid (renderBS, Html, toHtml, Term, term)
import Lucid.Html5
import Network.Wai (rawPathInfo, Request(queryString, rawQueryString))
import Network.HTTP.Types.URI (queryToQueryText, parseQueryText, renderQueryText, QueryText)
import Data.Binary.Builder (toLazyByteString, Builder)
import Network.URI.Encode (encodeTextWith, encodeText)
import Network.URI (isUnreserved)
import Data.Default (Default, def)
import Network.Wai (ResponseReceived)
import qualified Network.Wai as Wai

import Network.HTTP.Types (status200)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)

import Text.RawString.QQ



static :: (ScottyError e, Monad m) => Html () -> ActionT e m ()
static view =
  lucid view


-- -- Only accepts base paths, don't use params!
-- page :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
-- page = Scotty.matchAny

data Render = Render
  { embedJS :: Bool
  , toDoc :: Html () -> Html ()
  }

instance Default Render where
  def = Render True (simpleDocument "" "")

-- Always a load
-- handle
--   :: (MonadIO m, ScottyError e, ToJSON page)
--   => Render
--   -> page
--   -> PageHandler page (ActionT e m)
--   -> ActionT e m ()
-- handle cfg pg runPage = do
--   -- mps <- Params.fromParams <$> query
--   -- (mm, cmds) <- Runtime.parseBody =<< Scotty.body

--   res <- runPage pg Nothing []

--   respond cfg pg res



data YetiInit page = YetiInit
  { state :: Encoded 'Model
  , page :: page
  , delimiter :: Text
  } deriving (Generic, ToJSON)


-- this should be for the page to make sure they match!
pageUrl :: ToParams params => String -> params -> Text
pageUrl path ps =

  -- here, escape differently
  cs path <> "?" <> queryToText (Params.toParams ps)



respond :: forall page. ToJSON page => Render -> page -> Response -> (Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
respond (Render embJS toDocument) pg (Response encModel encParams view) respWai = do

  let yi = YetiInit encModel pg Encode.delimiter
  let content = Lucid.renderBS $ toDocument $ embedContent yi view

  respWai $ Wai.responseLBS status200 headers content

  where
    headers =
      [ ("X-Params", cs $ queryToText encParams)
      , ("Content-Type", "text/html")
      ]

    stateJSON :: ByteString
    stateJSON = Aeson.encode $ fromEncoded encModel

    embedStateScript :: YetiInit page -> Html ()
    embedStateScript yi = 
      script_ [type_ "text/javascript", id_ "yeti-state" ] $ Text.intercalate "\n"
        [ ""
        , "var yetiInit = " <> cs (Aeson.encode yi)
        , ""
        ]

    -- render the root node and embed the javascript
    embedContent :: YetiInit page -> Html () -> Html ()
    embedContent yi v = do
      "\n"
      embedStateScript yi
      "\n"
      div_ [id_ "yeti-root-content"] v
      "\n"
      when embJS $ do
        "\n"
        script_ [type_ "text/javascript"] JS.scripts

      -- DEBUGGING MODE
      -- script_ [type_ "text/javascript", src_ "/edom/build.js"] ("" :: Text)
      -- script_ [type_ "text/javascript", src_ "/edom/run.js"] ("" :: Text)

    -- vdom :: (ScottyError e, Monad m) => Text -> Html () -> ActionT e m ()
    -- vdom s h = do
    --   Scotty.setHeader "Content-Type" "application/vdom"
    --   Scotty.raw $
    --     cs s <> "\n" <> (Lucid.renderBS h)




-- | Convenience toDocument function to pass to render. Allows you to add stylesheets and javascript easily
simpleDocument :: Text -> Html () -> (Html () -> Html ())
simpleDocument t extra content = do
  html_ $ do
    head_ $ do
      title_ (toHtml t)
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]
      "\n"
      extra

    "\n"
    body_ $ do
      content



params :: (Monad m, ToParams p) => ActionT e m (Maybe p)
params = Params.fromParams <$> query



query :: (Monad m) => ActionT e m QueryText
query = do
  parseQueryText . rawQueryString <$> Scotty.request


-- renders a query text
-- but this escapes differently than I want
-- QueryText :: 
-- urlEncode :: Bool -> ByteString -> ByteString
-- type QueryText = [(Text, Maybe Text)]


-- 
queryToText :: QueryText -> Text
queryToText qt =
  -- first, 
  -- (cs $ toLazyByteString $ renderQueryText False qt)
  Text.intercalate "&" $ map segment qt
  where
    segment (k, Nothing) = key k
    segment (k, (Just v)) =

      -- we need to encode everything but special chars
      -- these are already escaped
      key k <> "=" <> urlEncode v
    
    -- encode the key
    key t = urlEncode t




lucid :: ScottyError e => Monad m => Html a -> ActionT e m ()
lucid h = do
  Scotty.setHeader "Content-Type" "text/html"
  Scotty.raw . Lucid.renderBS $ h



      



input' :: Term arg result => arg -> result
input' = term "input"