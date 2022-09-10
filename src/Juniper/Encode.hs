module Juniper.Encode where

import Juniper.Prelude
import GHC.Generics
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON(parseJSON), Value(..), Result(..), fromJSON, toJSON, GToJSON', GFromJSON, Zero, Options(..), defaultOptions, genericToJSON, SumEncoding(..), genericParseJSON)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector
import Data.Text (pack, intercalate, splitOn)


class LiveModel a where
  encodeModel :: a -> Text
  decodeModel :: Text -> Result a

  default encodeModel :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Text
  encodeModel = cs . Aeson.encode . genericToJSON defaultOptions

  default decodeModel :: (Generic a, GFromJSON Zero (Rep a)) => Text -> Result a
  decodeModel t =
    case Aeson.decode $ cs t of
      Nothing -> fail $ "Invalid JSON: " <> cs t
      Just val ->
        Aeson.parse (genericParseJSON defaultOptions) val

class LiveAction a where
  encodeAction' :: a -> EncodedAction
  decodeAction' :: EncodedAction -> Result a

  default encodeAction' :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> EncodedAction
  encodeAction' a =
    -- we should ALWAYS be able to map from this to an encoding
    let val = genericToJSON options a
    in case fromJSON val of
      Error err -> error $ "could not convert from JSON representation: " <> err <> " | " <> show val
      Success enc -> enc

  default decodeAction' :: (Generic a, GFromJSON Zero (Rep a)) => EncodedAction -> Result a
  decodeAction' e =
    Aeson.parse (genericParseJSON options) (toJSON e)

-- λ> :kind! (Rep Test)
-- (Rep Test) :: * -> *
-- = D1
--     ('MetaData "Test" "Juniper.Encode" "main" 'False)
--     (C1 ('MetaCons "Test" 'PrefixI 'False) U1)
-- data Test
--   = Test
--   deriving (Generic)


-- λ> :kind! (Rep Test1)
-- (Rep Test1) :: * -> *
-- = D1
--     ('MetaData "Test1" "Juniper.Encode" "main" 'False)
--     (C1
--        ('MetaCons "Test1" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--           (Rec0 Text)))
-- data Test1
--   = Test1 Text
--   deriving (Generic)



-- data Test
--   = TestA
--   | TestB Text
--   | TestC Bool Text
--   | TestI Info
--   deriving (Generic, Show)
-- instance (Encode ()) Test

-- data Info = Info { info :: Text }
--   deriving (Generic, Show)
-- instance ToJSON Info
-- instance FromJSON Info
-- instance Input Info where
--   def = Info ""


data Action = DoNothing
  deriving (Generic, Show)
-- instance ToJSON Action
instance LiveAction Action





-- λ> :kind! (Rep Woot)
-- (Rep Woot) :: * -> *
-- = D1
--     ('MetaData "Woot" "Juniper.Encode" "main" 'False)
--     (C1
--        ('MetaCons "Woot" 'PrefixI 'True)
--        (S1
--           ('MetaSel
--              ('Just "henry")
--              'NoSourceUnpackedness
--              'NoSourceStrictness
--              'DecidedLazy)
--           (Rec0 Text)))
-- data Woot = Woot
--   { henry :: Text }
--   deriving (Generic)
-- instance ToJSON Woot


-- data Big = Big Woot
--     deriving Generic



class Input a where
  empty :: a

instance Input Text where
  empty = ""

instance Input String where
  empty = ""

instance Input Bool where
  empty = False





-- A newtype for Value that only allows constructor serialization
data EncodedAction = EncodedAction
  { constructor :: Text
  , values :: [Value]
  } deriving (Generic, Show)

-- Decode the variants of generic JSON encoding to our Encoded type
instance FromJSON EncodedAction where
  parseJSON (Array v) = do

    -- Array [String "TestA",Array []]
    -- Array [String "TestB",String "woot"]
    -- Array [String "TestC",Array [Bool True,String "woot"]]

    case Vector.toList v of
      -- Single Tag
      (String c : [Array []]) ->
        pure $ EncodedAction c []

      -- Multiple Argument
      [String c, Array vs] ->
        pure $ EncodedAction c (Vector.toList vs)

      -- One Argument
      [String c, v1] ->
        pure $ EncodedAction c [v1]


      val -> fail $ "Expected [constructor, values...] but got: " <> show val

  parseJSON x = fail $ "Expected Array, but got: " <> show x

instance ToJSON EncodedAction where
  -- Array [String "TestA",Array []]
  -- Array [String "TestB",String "woot"]
  -- Array [String "TestC",Array [Bool True,String "woot"]]

  toJSON (EncodedAction c [v]) =
    Array (Vector.fromList [String c, v])

  toJSON (EncodedAction c vs) =
    Array (Vector.fromList [String c, Array (Vector.fromList vs)])


resultMaybe :: Result a -> Maybe a
resultMaybe (Error e) = Nothing
resultMaybe (Success a) = Just a

serialize :: EncodedAction -> Text
serialize (EncodedAction c vs) =
  intercalate "_|_"
    (c : map (cs . Aeson.encode) vs)

deserialize :: Text -> Maybe EncodedAction
deserialize t = do
  -- we need a better parser, or a better delimiter
  -- or to escape the spaces...
  c:vts <- pure $ splitOn "_|_" t
  vs <- mapM (Aeson.decode . cs) vts
  pure $ EncodedAction c vs



options :: Aeson.Options
options = defaultOptions
  -- force everything to follow the ["Constructor", []] format
  { tagSingleConstructors = True
  , allNullaryToStringTag = False
  , sumEncoding = TwoElemArray
  }


-- Encode an unapplied constructor accepting 1 input
encodeAction1' :: forall a inp. (LiveAction a, Input inp) => (inp -> a) -> EncodedAction
encodeAction1' f = 
  let EncodedAction c vs = encodeAction' (f empty :: a) :: EncodedAction
  in EncodedAction c (dropEnd vs)
  where
    dropEnd :: [x] -> [x]
    dropEnd [] = []
    dropEnd as = init as

encodeAction1 :: forall a inp. (LiveAction a, Input inp) => (inp -> a) -> Text
encodeAction1 = serialize . encodeAction1'

encodeAction :: LiveAction act => act -> Text
encodeAction = serialize . encodeAction'

decodeAction :: LiveAction act => Text -> Result act
decodeAction t =
  case deserialize t of
    Nothing -> Error $ "Could not parse action: " <> cs t
    Just ea -> decodeAction' ea







