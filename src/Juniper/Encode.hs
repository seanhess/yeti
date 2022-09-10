module Juniper.Encode where

import Juniper.Prelude
import GHC.Generics
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, Value, Result(..), fromJSON)
import Data.Text (pack, intercalate)




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
instance Input Info where
  def = Info ""




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






-- the only advantage to doing this is that we
-- can we encode to JSON instead?
-- we could use the generic JSON representation to get the tags, etc
-- and then still drop the last one

data Encoding t = Encoding
  { constructor :: Text
  , values :: [Value]
  }

instance Show (Encoding t) where
  show (Encoding c vs) =
    cs $ intercalate " "
      (c : map (cs . Aeson.encode) vs)

class Encode t a where
  encode :: a -> Encoding t
  decode :: Encoding t -> Maybe a

  default encode :: (Generic a, GenEncode (Rep a)) => a -> Encoding t
  encode a = genEncode (from a)

  default decode :: (Generic a, GenEncode (Rep a)) => Encoding t -> Maybe a
  decode e = to <$> genDecode e




-- Encode an unapplied constructor accepting 1 input
encode1 :: forall t a inp. (Encode t a, Input inp) => (inp -> a) -> Encoding t
encode1 f = 
  let Encoding c vs = encode (f def :: a) :: Encoding t
  in Encoding c (dropEnd vs)
  where
    dropEnd :: [x] -> [x]
    dropEnd [] = []
    dropEnd as = init as



class GenEncode f where
  genEncode :: f p -> Encoding t
  genDecode :: Encoding t -> Maybe (f p)

-- Datatype: Ignore, and encode contents
instance GenEncode f => GenEncode (M1 D d f) where
  genEncode (M1 x) = genEncode x
  genDecode e = M1 <$> genDecode e

-- Constructor: Encode constructor name in front of contents
instance (GenEncodeValue f, Constructor c) => GenEncode (M1 C c f) where
  -- how do we make the constructors match?
  genEncode c@(M1 x) = Encoding
    { constructor = pack (conName c)
    , values = genEncodeValue x
    }

  genDecode (Encoding c vss) = do
    -- check to see if we can parse it as THIS type
    guard (c == expectedConName)

    -- decode the rest of the arguments
    a <- decodeValues

    pure $ M1 a

    where
      -- the types are always different, so you can't use a fold
      decodeValues :: [Value] -> Maybe a
      decodeValues [] = Just (M1 U1)
      decodeValues = Nothing
    -- _ <- foldM combineValue (Just $ U1) vss

      -- the constructor name for this particular type
      expectedConName :: Text
      expectedConName = 
        let x = undefined :: M1 C c f x
        in pack (conName x)

      combineValue :: ((:*:) f g p) -> Value -> Maybe ((:*:) f g p)
      combineValue b v = do
        a <- fromResult (fromJSON v)
        pure $ a :*: b

      fromResult :: Result a -> Maybe a
      fromResult (Error e) = Nothing
      fromResult (Success a) = Just a





-- newtype M1 (i :: Type) (c :: Meta) (f :: k -> Type) (p :: k)
-- M1 S m f p


-- data Stuff = One | Two
instance (GenEncode a, GenEncode b) => GenEncode (a :+: b) where
  genEncode (L1 x) = genEncode x
  genEncode (R1 x) = genEncode x
  -- genDecode q = L1 <$> genDecode q <|> R1 <$> genDecode q






class GenEncodeValue f where
  genEncodeValue :: f p -> [Value]

-- Selector: Ignore metadata and encode contents
instance GenEncodeValue a => GenEncodeValue (M1 S s a) where
  genEncodeValue (M1 x) = genEncodeValue x

-- data Stuff = Stuff Text Bool
instance (GenEncodeValue a, GenEncodeValue b) => GenEncodeValue (a :*: b) where
  genEncodeValue (a :*: b) = genEncodeValue a ++ genEncodeValue b

-- Unary constructors
instance GenEncodeValue U1 where
  genEncodeValue x = [] 

instance (ToJSON a) => GenEncodeValue (K1 R a) where
  genEncodeValue (K1 a) = [Aeson.toJSON a]
