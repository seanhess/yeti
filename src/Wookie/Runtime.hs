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


data Response params = Response
  { resView :: Html ()
  , resParams :: params
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








-- One of the actions could be Load
data Command action
  = Init
  | Update action


type View model = (model -> Html ())
type Update model action m = (action -> StateT model m ())


-- | Load the page from route params, then apply the action
runAction
  :: forall m model params action. (MonadIO m, MonadFail m, PageAction action)
  => Page params model action m
  -> params
  -> Command action
  -> m (Response params)
runAction (Page params load update view) ps cmd = do

  -- load the initial model from the parameters
  m <- load ps

  -- run either a load or an action
  m' <- case cmd of
    Init -> pure m
    Update a -> execStateT (update a) m

  -- respond
  pure $ Response (view m') (params m')



command :: MonadFail m => PageAction action => ByteString -> m (Command action)
command "" = pure Init
command b =
  case readAction $ cs b of
    Just a -> pure $ Update a
    Nothing -> fail $ "Could not parse action: " <> cs b




-- | TODO do I need a type like this? Instead of all the typeclasses?
data Page params model action m = Page
  { params :: model -> params
  , load   :: params -> m model
  , update :: action -> StateT model m ()
  , view   :: model -> Html ()
  }


