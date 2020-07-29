{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Api where



import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.String.Conversions (cs)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (ToJSON, FromJSON(..), Value(..))
import GHC.Generics (Generic)
import Control.Monad.Trans.State.Lazy (StateT, modify, put, execStateT)
import Control.Lens (Lens', lens, (+=), (-=), (.=))
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5



-- Runtime -----------------------


data Message = Message
  { action :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON Message

data URL
  = Path Text
  | Num Integer
  deriving (Generic, Show, Eq)

showURL :: URL -> Text
showURL (Path s) = s
showURL (Num n) = cs $ show n


-- Just use monadfail if it doesn't work
class ToURL a where
  toURL :: a -> URL
  fromURL :: MonadFail m => URL -> m a

data Response = Response
  { resView :: Text
  , resUrl :: Text
  } deriving (Show, Generic)
instance ToJSON Response

parseURL :: Text -> URL
parseURL t =
  fromMaybe (Path t) (Num <$> readMaybe (cs t))

-- wait, this needs to run in IO
runtime :: Message -> IO Response
runtime Message{url, action} = do
  m <- fromURL (parseURL url) :: IO Model
  let a = read $ cs action :: Action
  m2 <- execStateT (update a) m :: IO Model
  let out = Lucid.renderBS (view m2)
  pure $ Response (cs out) (showURL $ toURL m2)


-- My Component --------------------

-- TODO make your own serialization, rather than Show / Read?
data Action
  = Increment
  | Init
  | Decrement
  | Set Integer
  deriving (Show, Read)



data Model = Model
  { _count :: Integer
  } deriving (Show, Eq)

count :: Lens' Model Integer
count = lens _count $ \record field -> record { _count = field }


instance ToURL Model where
  toURL (Model m) = Num m
  fromURL (Num m) = pure $ Model m
  fromURL (Path "") = pure $ Model 0
  fromURL v = fail $ "ToURL: Expected Number, but got " <> show v


update :: Action -> StateT Model IO ()
update Init = pure ()
update Increment = count += 1
update Decrement = count -= 1
update (Set n) = count .= n


-- TODO views should just be data, so vdom can handle it
view :: Model -> Html ()
view m = div_ $ do
  div_ $ do
    h1_ "Counter"
    div_ (toHtml $ show $ _count m)
    button_ [ onclick_ (serializeAction Increment)] "Increment"
    button_ [ onclick_ (serializeAction Decrement)] "Decrement"
    button_ [ onclick_ (serializeAction (Set 5))] "Set 5"


-- well, let's see, it can call it via the whole url!
serializeAction :: Action -> Text
serializeAction a = "runtime('"<> (cs $ show a) <> "')"


