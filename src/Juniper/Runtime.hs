module Juniper.Runtime where


import Juniper.Prelude
import Juniper.Page (Page(..), PageAction(..))
import Juniper.State as State (ToState(..))

import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.ByteString.Lazy as BSL
import Lucid (Html, renderBS)
import Text.Read (readMaybe)
import Data.Map ((!?))
import Control.Monad (foldM)



data Response params = Response
  { resView :: Html ()
  , resParams :: params
  } deriving (Show, Generic)



-- class Page model params where
--   toParams :: model -> params
--   loadPage :: (MonadIO m, MonadFail m) => params -> m model






data Command action
  = Submit
  | Update action


run
  :: (Monad m, ToState model, PageAction action)
  => Page params model action m
  -> Maybe params
  -> Maybe model
  -> [Command action]
  -> m model
run pg mps Nothing _     = runLoad pg mps
run pg mps (Just m) cmds = runActions pg m cmds

-- if we only have params, no model, and no commands
runLoad
  :: forall m model params action. (Monad m, PageAction action)
  => Page params model action m
  -> Maybe params 
  -> m model
runLoad (Page params load update view) ps = do
  load ps


-- we can only run actions if we already have a model
runActions
  :: forall m model params action. (Monad m, PageAction action)
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




parseBody :: (MonadIO m, MonadFail m, PageAction action, ToState model) => ByteString -> m (Maybe model, [Command action])
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


parseModel :: (MonadFail m, MonadIO m, ToState model) => ByteString -> m model
parseModel inp = do
  case State.decode (cs inp) of
    Nothing -> fail $ "Could not parse model: " <> cs inp
    Just m -> pure m



parseCommand :: (MonadFail m, PageAction action) => ByteString -> m (Command action)
parseCommand "|Submit|" = pure Submit
parseCommand t =
  case readAction (cs t) of
    Just a -> pure $ Update a
    Nothing -> fail $ "Could not parse action: " <> cs t


