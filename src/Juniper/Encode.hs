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



data Test
  = TestA
  | TestB Text
  | TestC Bool Text
  | TestI Info
  deriving (Generic, Show)
instance (Encode ()) Test

data Info = Info { info :: Text }
  deriving (Generic, Show)
instance ToJSON Info
instance FromJSON Info
instance Input Info where
  def = Info ""


data Action = DoNothing
  deriving (Generic, Show)
instance ToJSON Action
instance (Encode ()) Action



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
  def :: a

instance Input Text where
  def = ""

instance Input String where
  def = ""

instance Input Bool where
  def = False





-- A newtype for Value that only allows constructor serialization
data Encoding t = Encoding
  { constructor :: Text
  , values :: [Value]
  } deriving (Generic, Show)

-- Decode the variants of generic JSON encoding to our Encoding type
instance FromJSON (Encoding t) where
  parseJSON (Array v) = do

    -- Array [String "TestA",Array []]
    -- Array [String "TestB",String "woot"]
    -- Array [String "TestC",Array [Bool True,String "woot"]]

    case Vector.toList v of
      -- Single Tag
      (String c : [Array []]) ->
        pure $ Encoding c []

      -- Multiple Argument
      [String c, Array vs] ->
        pure $ Encoding c (Vector.toList vs)

      -- One Argument
      [String c, v1] ->
        pure $ Encoding c [v1]


      val -> fail $ "Expected [constructor, values...] but got: " <> show val

  parseJSON x = fail $ "Expected Array, but got: " <> show x

instance ToJSON (Encoding t) where
  -- Array [String "TestA",Array []]
  -- Array [String "TestB",String "woot"]
  -- Array [String "TestC",Array [Bool True,String "woot"]]

  toJSON (Encoding c [v]) =
    Array (Vector.fromList [String c, v])

  toJSON (Encoding c vs) =
    Array (Vector.fromList [String c, Array (Vector.fromList vs)])


resultMaybe :: Result a -> Maybe a
resultMaybe (Error e) = Nothing
resultMaybe (Success a) = Just a

serialize :: Encoding t -> Text
serialize (Encoding c vs) =
  intercalate " "
    (c : map (cs . Aeson.encode) vs)

deserialize :: Text -> Maybe (Encoding t)
deserialize t = do
  c:vts <- pure $ splitOn " " t
  vs <- mapM (Aeson.decode . cs) vts
  pure $ Encoding c vs

class Encode t a where
  encode :: a -> Encoding t
  decode :: Encoding t -> Result a

  default encode :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Encoding t
  encode = genEncode

  default decode :: (Generic a, GFromJSON Zero (Rep a)) => Encoding t -> Result a
  decode = genDecode


genEncode :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Encoding t
genEncode a =
  -- we should ALWAYS be able to map from this to an encoding
  let val = genericToJSON options a
  in case fromJSON val of
    Error err -> error $ "could not convert from JSON representation: " <> err <> " | " <> show val
    Success enc -> enc

genDecode :: (Generic a, GFromJSON Zero (Rep a)) => Encoding t -> Result a
genDecode e =
  Aeson.parse (genericParseJSON options) (toJSON e)

options :: Aeson.Options
options = defaultOptions
  -- force everything to follow the ["Constructor", []] format
  { tagSingleConstructors = True
  , allNullaryToStringTag = False
  , sumEncoding = TwoElemArray
  }


-- Encode an unapplied constructor accepting 1 input
encode1 :: forall t a inp. (Encode t a, Input inp) => (inp -> a) -> Encoding t
encode1 f = 
  let Encoding c vs = encode (f def :: a) :: Encoding t
  in Encoding c (dropEnd vs)
  where
    dropEnd :: [x] -> [x]
    dropEnd [] = []
    dropEnd as = init as



-- class GenEncode f where
--   genEncode :: f p -> Encoding t
--   genDecode :: Encoding t -> Maybe (f p)

-- -- Datatype: Ignore, and encode contents
-- instance GenEncode f => GenEncode (M1 D d f) where
--   genEncode (M1 x) = genEncode x
--   genDecode e = M1 <$> genDecode e

-- -- Constructor: Encode constructor name in front of contents
-- instance (GenEncodeValue f, Constructor c) => GenEncode (M1 C c f) where
--   -- how do we make the constructors match?
--   genEncode c@(M1 x) = Encoding
--     { constructor = pack (conName c)
--     , values = genEncodeValue x
--     }

--   genDecode (Encoding c vss) = do
--     -- check to see if we can parse it as THIS type
--     guard (c == expectedConName)

--     -- decode the rest of the arguments
--     a <- decodeValues

--     pure $ M1 a

--     where
--       -- the types are always different, so you can't use a fold
--       decodeValues :: [Value] -> Maybe a
--       decodeValues [] = Just (M1 U1)
--       decodeValues = Nothing
--     -- _ <- foldM combineValue (Just $ U1) vss

--       -- the constructor name for this particular type
--       expectedConName :: Text
--       expectedConName = 
--         let x = undefined :: M1 C c f x
--         in pack (conName x)

--       combineValue :: ((:*:) f g p) -> Value -> Maybe ((:*:) f g p)
--       combineValue b v = do
--         a <- fromResult (fromJSON v)
--         pure $ a :*: b

--       fromResult :: Result a -> Maybe a
--       fromResult (Error e) = Nothing
--       fromResult (Success a) = Just a





-- -- newtype M1 (i :: Type) (c :: Meta) (f :: k -> Type) (p :: k)
-- -- M1 S m f p


-- -- data Stuff = One | Two
-- instance (GenEncode a, GenEncode b) => GenEncode (a :+: b) where
--   genEncode (L1 x) = genEncode x
--   genEncode (R1 x) = genEncode x
--   -- genDecode q = L1 <$> genDecode q <|> R1 <$> genDecode q






-- class GenEncodeValue f where
--   genEncodeValue :: f p -> [Value]

-- -- Selector: Ignore metadata and encode contents
-- instance GenEncodeValue a => GenEncodeValue (M1 S s a) where
--   genEncodeValue (M1 x) = genEncodeValue x

-- -- data Stuff = Stuff Text Bool
-- instance (GenEncodeValue a, GenEncodeValue b) => GenEncodeValue (a :*: b) where
--   genEncodeValue (a :*: b) = genEncodeValue a ++ genEncodeValue b

-- -- Unary constructors
-- instance GenEncodeValue U1 where
--   genEncodeValue x = [] 

-- instance (ToJSON a) => GenEncodeValue (K1 R a) where
--   genEncodeValue (K1 a) = [Aeson.toJSON a]
