{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Wookie.Runtime where



import Control.Monad.State.Lazy (StateT, execStateT)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import GHC.Generics (Generic)
import Lucid (Html, renderBS)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, (!?))
import Data.Maybe (mapMaybe)
import Data.Function ((&))


data Message = Message
  { action :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON Message


data Response = Response
  { resView :: Html ()
  , resParams :: Text
  } deriving (Show, Generic)



-- class Page model params where
--   toParams :: model -> params
--   loadPage :: (MonadIO m, MonadFail m) => params -> m model



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



-- TODO use megaparsec
class Params a where
  encode :: a -> Text
  decode :: Text -> Maybe a

instance Params Integer where
  encode = cs . show
  decode = readMaybe . cs

instance Params Int where
  encode = cs . show
  decode = readMaybe . cs

instance Params Text where
  encode = id
  decode = Just . id

instance Params () where
  encode _ = ""
  decode _ = Just ()

-- this will decode Maybe Text as ""
instance Params a => Params (Maybe a) where
  encode (Just a) = encode a
  encode Nothing = ""

  decode "" = Just $ Nothing
  decode a = Just $ decode a


instance (Params a, Params b) => Params (a, b) where
  encode (a, b) = Text.intercalate "&" $ [encode a, encode b]
  decode t = do
    [at, bt] <- pure $ Text.splitOn "&" t
    a <- decode at
    b <- decode bt
    pure (a, b)


instance Params a => Params [a] where
  encode [] = "[]"
  encode as = "[" <> Text.intercalate "," (fmap encode as) <> "]"

  -- they MUST have [ and ] around them?
  -- maybe I should use a real parser...
  decode "[]" = Just []
  decode t = Just $ t
    & Text.dropAround isBracket
    & Text.splitOn ","
    & mapMaybe decode
    where isBracket c = c == '[' || c == ']'






-- One of the actions could be Load
data Command action
  = Load
  | Action action


type View model = (model -> Html ())
type Update model action m = (action -> StateT model m ())


-- | Load the page from route params, then apply the action
runAction
  :: forall m model params action. (MonadIO m, MonadFail m, Params params, PageAction action)
  => Page params model action m
  -> params
  -> Command action
  -> m Response
runAction (Page params load update view) ps cmd = do

  -- load the initial model from the parameters
  m <- load ps

  -- run either a load or an action
  m' <- case cmd of
    Load -> pure m
    Action a -> execStateT (update a) m

  -- respond
  let ps' = params m' :: params
  pure $ Response (view m') (encode ps')


command :: MonadFail m => PageAction action => ByteString -> m (Command action)
command "" = pure Load
command b =
  case readAction $ cs b of
    Just a -> pure $ Action a
    Nothing -> fail $ "Could not parse action: " <> cs b




-- | TODO do I need a type like this? Instead of all the typeclasses?
data Page params model action m = Page
  { params :: model -> params
  , load   :: params -> m model
  , update :: action -> StateT model m ()
  , view   :: model -> Html ()
  }


