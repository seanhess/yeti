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



class Page model params where
  toParams :: model -> params
  loadPage :: (MonadIO m, MonadFail m) => params -> m model



class PageAction a where
  showAction :: a -> Text
  readAction :: Text -> Maybe a

  default showAction :: Show a => a -> Text
  showAction = cs . show

  default readAction :: Read a => Text -> Maybe a
  readAction = readMaybe . cs




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


-- type Key = TL.Text
-- type Params = Map Key TL.Text


-- -- allows me to override the read instance
-- -- unless I want to just make a list type and use the normal read?
-- class Param a where
--   readParam :: TL.Text -> Maybe a

-- instance Param Integer where
--   readParam = readMaybe . cs

-- instance Param Text where
--   readParam = Just . cs

-- -- instance Param a => Param (Maybe a) where
-- --   readParam = readParam


-- required :: (MonadFail m, Param a) => Key -> Params -> m a
-- required k ps = do
--   v <- ps !? k      & failMaybe ("Missing key: " <> cs k)
--   readParam (cs v) & failMaybe ("Could not read:" <> show (k, v))


-- -- | looks up the param and gives you a maybe if it failed
-- param :: Param a => Key -> Params -> Maybe a
-- param k ps = do
--   v <- ps !? k
--   readParam (cs v)



-- failMaybe :: MonadFail m => String -> Maybe a -> m a
-- failMaybe _ (Just v) = pure v
-- failMaybe err Nothing = fail err

-- -- Looks up the param for you and fails if it can't find it
-- paramFindKey :: MonadFail m => Key -> Params -> m Text
-- paramFindKey k ps = do
--   case ps !? k of
--     Nothing -> fail $ "Missing key: " <> cs k
--     Just v -> pure $ cs v

-- paramInt :: MonadFail m => Key -> Params -> m Integer
-- paramInt k ps = do
--   v <- paramFindKey k ps
--   case readMaybe (cs v) of
--     Nothing -> fail $ "Could not read: " <> show (k, v)
--     Just n -> pure n




-- | Load the page from route params
-- runLoad
--   :: (MonadIO m, Page model params, Params params)
--   -> (model -> Html ())
--   => params
--   -> m (Html ())
-- runLoad view p = do
--    m <- liftIO $ loadPage p
--    pure $ view m


-- One of the actions could be Load
data Command action
  = Load
  | Action action


type View model = (model -> Html ())


-- | Load the page from route params, then apply the action
runAction
  :: forall m model params action. (MonadIO m, MonadFail m, Page model params, Params params, PageAction action)
  => (action -> StateT model m ())
  -> View model
  -> params
  -> Command action
  -> m Response
runAction update view ps cmd = do

  -- load the initial model from the parameters
  m <- loadPage ps

  -- run either a load or an action
  m' <- case cmd of
    Load -> pure m
    Action a -> execStateT (update a) m

  -- respond
  let ps' = toParams m' :: params
  pure $ Response (view m') (encode ps')


command :: MonadFail m => PageAction action => ByteString -> m (Command action)
command "" = pure Load
command b =
  case readAction $ cs b of
    Just a -> pure $ Action a
    Nothing -> fail $ "Could not parse action: " <> cs b




-- | TODO do I need a type like this? Instead of all the typeclasses?
-- data Page params model action = Page
--   { load :: params -> IO model
--   , update :: action -> StateT model IO ()
--   , view :: model -> Html ()
--   }


