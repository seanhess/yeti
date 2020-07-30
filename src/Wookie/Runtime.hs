{-# LANGUAGE DeriveGeneric #-}
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
import Data.Text as Text (Text)
import GHC.Generics (Generic)
import Lucid (Html, renderBS)
import Text.Read (readMaybe)


data Message = Message
  { action :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON Message


data Response = Response
  { resView :: Html ()
  -- , resUrl :: Text
  } deriving (Show, Generic)



-- 1. Loads the model (from url parameters)
-- 2. figures out the action (from the body) 
-- 3. applies the action via update
-- 4. renders the view

runtime :: forall model action. (Read action, Show action) => IO model -> (action -> StateT model IO ()) -> (model -> Html ()) -> ByteString -> IO Response
runtime load update view body = do
  m <- load :: IO model

  -- do we have a matching action?
  -- TODO this is a dumb way, this should only happen on GETS
  -- if we expected to have an action and it fails we should error out
  let ma = readMaybe $ cs body :: Maybe action
  print (body, ma)
  m2 <- case ma of
    Just a -> execStateT (update a) m :: IO model
    Nothing -> pure m

  let h = view m2
  pure $ Response h -- (renderSegment $ toSegment m2)


data Page params model action = Page
  { load :: params -> IO model
  , update :: action -> StateT model IO ()
  , view :: model -> Html ()
  }


-- I don't have a good way to lookup the page!
-- Grrr....
-- I mean.... Wait, no I could just return applied runtimes
-- it's just ONE cycle, not

