{-# LANGUAGE ConstraintKinds #-}
module Yeti.Runtime where

import Yeti.Prelude

import Control.Monad (foldM)
import Control.Exception (Exception, throw)
import Data.Aeson (ToJSON, FromJSON, Result(..))
import Data.ByteString.Lazy (ByteString)
import Data.List as List (take)
import Data.Map ((!?))
import Lucid (Html, renderBS)
import Network.HTTP.Types.URI (QueryText)
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL (Text)
import Yeti.Encode
import Yeti.Params (ToParams(..))



-- This needs to be encoded already
data Response = Response
  { resModel :: Encoded 'Model
  , resParams :: QueryText
  , resView :: Html ()
  } deriving (Show, Generic)


type PageHandler page m = page -> Maybe (Encoded 'Model) -> QueryText -> [Encoded 'Action] -> m Response

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


runPage
  :: forall m model params action. (MonadIO m, LiveAction action, LiveModel model, ToParams params)
  => Page params model action m
  -> Maybe (Encoded 'Model)
  -> QueryText
  -> [Encoded 'Action]
  -> m Response
runPage pg Nothing qt _ = do
  let mps = fromParams qt
  m <- (load pg) mps
  pure $ response pg m

runPage pg (Just encModel) _ as = do
  cmds <- mapM parseCommand as :: m [Command action]
  m <- parseModel encModel
  m' <- runActions pg m cmds
  pure $ response pg m'


response :: (LiveModel model, ToParams params) => Page params model action m -> model -> Response
response (Page params _ _ view) m = Response (encodeModel m) (toParams $ params m) (view m)


runCommand :: (Monad m) => (action -> model -> m model) -> model -> Command action -> m model
runCommand update m cmd =
  case cmd of
    Submit -> pure m
    Update a -> update a m


parseBody :: (MonadIO m, LiveAction action, LiveModel model) => ByteString -> m (Maybe model, [Command action])
parseBody body = do
  case BSL.split newline body of
    [] -> pure (Nothing, [])
    (ml:cls) -> do
      -- the first line is always the model, you can't run actions without it
      m <- parseModel $ Encoded $ cs ml

      -- each other line contains an action
      cmds <- mapM (parseCommand . Encoded . cs) cls

      pure (Just m, cmds)
  where newline = 10 -- fromEnum '\n'


parseModel :: (MonadIO m, LiveModel model) => Encoded 'Model -> m model
parseModel enc = do
  case decodeModel enc of
    Error e -> throw $ NoParseModel enc
    Success m -> pure m

parseCommand :: (MonadIO m, LiveAction action) => Encoded 'Action -> m (Command action)
parseCommand e = do

  let c:vts = pure $ Text.splitOn "\t" (fromEncoded e)
  print (c, vts)

  case decodeAction e of
    Success (a :: action) -> pure $ Update a
    Error err -> throw $ NoParseAction e



data Error
  = NoParseModel (Encoded 'Model)
  | NoParseAction (Encoded 'Action)
  deriving (Show, Eq, Exception)


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

