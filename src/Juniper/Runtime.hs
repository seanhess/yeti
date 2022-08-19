{-# LANGUAGE ConstraintKinds #-}
module Juniper.Runtime where

import Juniper.Prelude

import qualified Data.Aeson as Aeson
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
  :: (Monad m, Encode LiveModel model, Encode LiveAction action)
  => Page params model action m
  -> Maybe params
  -> Maybe model
  -> [Command action]
  -> m model
run pg mps Nothing _     = runLoad pg mps
run pg mps (Just m) cmds = runActions pg m cmds

-- if we only have params, no model, and no commands
runLoad
  :: forall m model params action. (Monad m, Encode LiveAction action)
  => Page params model action m
  -> Maybe params 
  -> m model
runLoad (Page params load update view) ps = do
  load ps


-- we can only run actions if we already have a model
runActions
  :: forall m model params action. (Monad m, Encode LiveAction action)
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


parseBody :: (MonadIO m, MonadFail m, Encode LiveAction action, Encode LiveModel model) => ByteString -> m (Maybe model, [Command action])
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


parseModel :: (MonadFail m, MonadIO m, Read model, Encode LiveModel model) => ByteString -> m model
parseModel inp = do
  case decode (Encoded $ cs inp :: Encoded LiveModel) of
    Nothing -> fail $ "Could not parse model: " <> cs inp
    Just m -> pure m



parseCommand :: (MonadFail m, Show action, Encode LiveAction action) => ByteString -> m (Command action)
parseCommand "|Submit|" = pure Submit
parseCommand t =
  case decode (Encoded $ cs t :: Encoded LiveAction) of
    Just (a :: action) -> pure $ Update a
    Nothing -> fail $ "Could not parse action: " <> cs t





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


newtype Encoded a = Encoded { fromEncoded :: Text }

-- | Must be show/read, you can't customize it
class (Show a, Read a) => Encode typ a

encode :: Encode typ a => a -> Encoded typ
encode m = Encoded $ cs $ show m

decode :: Encode typ a => Encoded typ -> Maybe a
decode e = readMaybe $ cs $ fromEncoded e

-- | Encodes a constructor that takes one argument. Removes the last argument, ready to accept new ones
encode1 :: (Encode typ a, Show x) => (x -> a) -> x -> Encoded typ
encode1 con x =
  let tot = show (con x)
      end = show x
  in Encoded $ Text.stripEnd $ cs $ List.take (length tot - length end) tot

data LiveModel
data LiveAction
