module Yeti.Encode where

import Yeti.Prelude
import GHC.Generics hiding (Constructor)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON(parseJSON), Value(..), Result(..), fromJSON, toJSON, GToJSON', GFromJSON, Zero, Options(..), defaultOptions, genericToJSON, SumEncoding(..), genericParseJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import Data.String (IsString)


data Encoding
  = Model
  | Params
  | Action

class LiveModel a where
  encodeModel :: a -> Encoded 'Model
  decodeModel :: Encoded 'Model -> Result a

  default encodeModel :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Encoded 'Model
  encodeModel = Encoded . cs . Aeson.encode . genericToJSON defaultOptions

  default decodeModel :: (Generic a, GFromJSON Zero (Rep a)) => Encoded 'Model -> Result a
  decodeModel (Encoded t) =
    case Aeson.decode $ cs t of
      Nothing -> Error $ "Invalid JSON Value: " <> cs t
      Just val ->
        Aeson.parse (genericParseJSON defaultOptions) val


instance LiveModel ()


class LiveAction a where
  toConstructor :: a -> Constructor
  fromConstructor :: Constructor -> Result a

  default toConstructor :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Constructor
  toConstructor a =
    -- we should ALWAYS be able to map from this to an encoding
    let val = genericToJSON options a
    in case fromJSON val of
      Error err -> error $ "could not convert from JSON representation: " <> err <> " | " <> show val
      Success enc -> enc

  default fromConstructor :: (Generic a, GFromJSON Zero (Rep a)) => Constructor -> Result a
  fromConstructor e =
    Aeson.parse (genericParseJSON options) (toJSON e)

-- λ> :kind! (Rep Test)
-- (Rep Test) :: * -> *
-- = D1
--     ('MetaData "Test" "Yeti.Encode" "main" 'False)
--     (C1 ('MetaCons "Test" 'PrefixI 'False) U1)
-- data Test
--   = Test
--   deriving (Generic)


-- λ> :kind! (Rep Test1)
-- (Rep Test1) :: * -> *
-- = D1
--     ('MetaData "Test1" "Yeti.Encode" "main" 'False)
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


-- data Action = DoNothing
--   deriving (Generic, Show)
-- -- instance ToJSON Action
-- instance LiveAction Action


instance LiveAction ()





-- λ> :kind! (Rep Woot)
-- (Rep Woot) :: * -> *
-- = D1
--     ('MetaData "Woot" "Yeti.Encode" "main" 'False)
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



newtype Encoded a = Encoded { fromEncoded :: Text }
  deriving newtype (Show, IsString, Eq, ToJSON)


-- A newtype for Value that only allows constructor serialization
data Constructor = Constructor
  { constructor :: Text
  , values :: [Value]
  } deriving (Generic, Show)

-- Decode the variants of generic JSON encoding to our Encoded type
instance FromJSON Constructor where
  parseJSON (Array v) = do

    -- Array [String "TestA",Array []]
    -- Array [String "TestB",String "woot"]
    -- Array [String "TestC",Array [Bool True,String "woot"]]

    case Vector.toList v of
      -- Single Tag
      (String c : [Array []]) ->
        pure $ Constructor c []

      -- Multiple Argument
      [String c, Array vs] ->
        pure $ Constructor c (Vector.toList vs)

      -- One Argument
      [String c, v1] ->
        pure $ Constructor c [v1]

      val -> fail $ "Expected [constructor, values...] but got: " <> show val

  parseJSON x = fail $ "Expected Array, but got: " <> show x

instance ToJSON Constructor where
  -- Array [String "TestA",Array []]
  -- Array [String "TestB",String "woot"]
  -- Array [String "TestC",Array [Bool True,String "woot"]]

  toJSON (Constructor c [v]) =
    Array (Vector.fromList [String c, v])

  toJSON (Constructor c vs) =
    Array (Vector.fromList [String c, Array (Vector.fromList vs)])


resultMaybe :: Result a -> Maybe a
resultMaybe (Error e) = Nothing
resultMaybe (Success a) = Just a


serialize :: Constructor -> Encoded 'Action
serialize (Constructor c []) =
  Encoded c
serialize (Constructor c vs) =
  Encoded $ c <> " " <> (cs $ Aeson.encode vs)

deserialize :: Encoded 'Action -> Maybe Constructor
deserialize (Encoded t) = do
  let (con, rest) = Text.breakOn " " t
  let vse = Text.stripStart rest
  vs <- decodeArgs vse
  pure $ Constructor con vs
  where
    decodeArgs "" = pure []
    decodeArgs x = Aeson.decode (cs x)



options :: Aeson.Options
options = defaultOptions
  -- force everything to follow the ["Constructor", []] format
  { tagSingleConstructors = True
  , allNullaryToStringTag = False
  , sumEncoding = TwoElemArray
  }


-- Encode an unapplied constructor accepting 1 input
toConstructor1 :: forall a inp. (LiveAction a, Input inp) => (inp -> a) -> Constructor
toConstructor1 f = 
  let Constructor c vs = toConstructor (f empty :: a) :: Constructor
  in Constructor c (dropEnd vs)
  where
    dropEnd :: [x] -> [x]
    dropEnd [] = []
    dropEnd as = init as

encodeAction1 :: forall a inp. (LiveAction a, Input inp) => (inp -> a) -> Encoded 'Action
encodeAction1 = serialize . toConstructor1

encodeAction :: LiveAction act => act -> Encoded 'Action
encodeAction = serialize . toConstructor

decodeAction :: LiveAction act => Encoded 'Action -> Result act
decodeAction e =
  case deserialize e of
    Nothing -> Error $ "Could not parse action: " <> cs (fromEncoded e)
    Just ea -> fromConstructor ea









-- encoding: 
-- constructorName<SPACE>[Aeson Params]