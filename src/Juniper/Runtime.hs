{-# LANGUAGE ConstraintKinds #-}
module Juniper.Runtime where

import Juniper.Prelude

import Juniper.Encode
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, Result(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.ByteString.Lazy as BSL
import Data.List as List (take)
import Lucid (Html, renderBS)
import Text.Read (readMaybe)
import Data.Map ((!?))
import Control.Monad (foldM)



data Response params = Response
  { resView :: Html ()
  , resParams :: params
  } deriving (Show, Generic)



data Command action
  = Submit
  | Update action


run
  :: (Monad m, LiveModel model, LiveAction action)
  => Page params model action m
  -> Maybe params
  -> Maybe model
  -> [Command action]
  -> m model
run pg mps Nothing _     = runLoad pg mps
run pg mps (Just m) cmds = runActions pg m cmds

-- if we only have params, no model, and no commands
runLoad
  :: forall m model params action. (Monad m, LiveAction action)
  => Page params model action m
  -> Maybe params 
  -> m model
runLoad (Page params load update view) ps = do
  load ps


-- we can only run actions if we already have a model
runActions
  :: forall m model params action. (Monad m, LiveAction action)
  => Page params model action m
  -> model
  -> [Command action]
  -> m model
runActions (Page params load update view) m cmds = do
  foldM (runCommand update) m cmds


response :: Page params model action m -> model -> Response params
response (Page params _ _ view) m = Response (view m) (params m)


runCommand :: (Monad m) => (action -> model -> m model) -> model -> Command action -> m model
runCommand update m cmd =
  case cmd of
    Submit -> pure m
    Update a -> update a m


parseBody :: (MonadIO m, MonadFail m, LiveAction action, LiveModel model) => ByteString -> m (Maybe model, [Command action])
parseBody body = do
  case BSL.split newline body of
    [] -> pure (Nothing, [])
    (ml:cls) -> do
      -- the first line is always the model, you can't run actions without it
      m <- parseModel ml

      -- each other line contains an action
      cmds <- mapM parseCommand cls

      pure (Just m, cmds)
  where newline = 10 -- fromEnum '\n'


parseModel :: (MonadFail m, MonadIO m, LiveModel model) => ByteString -> m model
parseModel inp = do
  case decodeModel (cs inp) of
    Error e -> fail $ "Could not parse model: " <> cs inp <> " " <> e
    Success m -> pure m



parseCommand :: (MonadFail m, MonadIO m, LiveAction action) => ByteString -> m (Command action)
parseCommand "|Submit|" = pure Submit
parseCommand t =
  case decodeAction (cs t) of
    Success (a :: action) -> pure $ Update a
    Error err -> fail $ "Could not parse action: " <> cs t <> " " <> err





data Page params model action m = Page
  { params :: Params params model
  , load   :: Load params model m
  , update :: Update action model m
  , view   :: View          model
  }


type Load   params model m = Maybe params -> m model
type Params params model   = model -> params
type Update action model m = action -> model -> m model
type View          model   = model -> Html ()


-- a page without params
simplePage
  :: forall action model m. Applicative m
  => m model
  -> Update action model m
  -> View model
  -> Page () model action m
simplePage int up vw = Page (const ()) (const int) up vw

