{-# LANGUAGE ConstraintKinds #-}
module Yeti.Runtime where

import Yeti.Prelude

import Control.Exception (Exception, throw)
import Control.Monad (foldM)
import Data.Aeson (Result(..))
import Network.HTTP.Types.URI (QueryText)
import Yeti.Encode
    ( decodeAction,
      Encoded(fromEncoded),
      Encoding(Action, Model),
      LiveAction,
      LiveModel(..) )
import Yeti.Page (Page(..), Response(..))
import Yeti.Params (ToParams(..))
import qualified Data.Text as Text


-- we can only run actions if we already have a model
runActions
  :: forall m model params action. (Monad m, LiveAction action)
  => Page params model action m
  -> model
  -> [action]
  -> m model
runActions (Page params load update view) m acts = do
  foldM (flip update) m acts



run
  :: forall m model params action. (MonadIO m, LiveAction action, LiveModel model, ToParams params)
  => Page params model action m
  -> Maybe (Encoded 'Model)
  -> QueryText
  -> [Encoded 'Action]
  -> m Response
run pg Nothing qt _ = do
  let mps = fromParams qt
  m <- (load pg) mps
  pure $ response pg m

run pg (Just encModel) _ as = do
  cmds <- mapM parseAction as :: m [action]
  m <- parseModel encModel
  m' <- runActions pg m cmds
  pure $ response pg m'


response :: (LiveModel model, ToParams params) => Page params model action m -> model -> Response
response (Page params _ _ view) m = Response (encodeModel m) (toParams $ params m) (view m)


parseModel :: (MonadIO m, LiveModel model) => Encoded 'Model -> m model
parseModel enc = do
  case decodeModel enc of
    Error e -> throw $ NoParseModel enc
    Success m -> pure m

parseAction :: (MonadIO m, LiveAction action) => Encoded 'Action -> m action
parseAction e = do

  let c:vts = pure $ Text.splitOn "\t" (fromEncoded e)
  print (c, vts)

  case decodeAction e of
    Success (a :: action) -> pure a
    Error err -> throw $ NoParseAction e



data Error
  = NoParseModel (Encoded 'Model)
  | NoParseAction (Encoded 'Action)
  deriving (Show, Eq, Exception)
