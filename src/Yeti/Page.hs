module Yeti.Page where

import Yeti.Prelude
import Yeti.Encode (Encoded(..), Encoding(Model, Action))
import Yeti.Params (QueryText)
import Data.Aeson
import Data.Text as Text (toLower, intercalate)
import Text.Read (readMaybe)
import GHC.Generics
import Lucid (Html)






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

type StaticPage m = Page () () () m
type SimplePage model action m = Page () model action m


-- a page without params
simplePage
  :: forall action model m. Applicative m
  => m model
  -> Update action model m
  -> View model
  -> Page () model action m
simplePage int up vw = Page (const ()) (const int) up vw


staticPage :: Applicative m => Html () -> Page () () () m
staticPage view = Page (const ()) (\_ -> pure ()) (\_ _ -> pure ()) (const view)




type PageHandler page m = page -> Maybe (Encoded 'Model) -> QueryText -> [Encoded 'Action] -> m Response



-- This needs to be encoded already
data Response = Response
  { resModel :: Encoded 'Model
  , resParams :: QueryText
  , resView :: Html ()
  } deriving (Show, Generic)



pageUrlPath :: (RoutePage page) => page -> Text
pageUrlPath p = "/" <> intercalate "/" (pageRoute p)


-- TODO better encoding
-- TODO generalized!
class (Show page, ToJSON page, FromJSON page) => RoutePage page where
  routePage :: [Text] -> Maybe page
  pageRoute :: page -> [Text]

  default routePage :: (Generic page, GenRoute (Rep page)) => [Text] -> Maybe page
  routePage paths = to <$> genRoutePage paths

  default pageRoute :: (Generic page, GenRoute (Rep page)) => page -> [Text]
  pageRoute p = genPageRoute $ from p

class GenRoute f where
  genRoutePage :: [Text] -> Maybe (f p)
  genPageRoute :: (f p) -> [Text]


-- datatype metadata
instance (GenRoute f) => GenRoute (M1 D c f) where
  genRoutePage ps = M1 <$> genRoutePage ps
  genPageRoute (M1 x) = genPageRoute x

-- Constructor names / lines
instance (Constructor c, GenRoute f) => GenRoute (M1 C c f) where
  genRoutePage (n:ps) = do
    -- take the first path off the list
    -- check that it matches the constructor name
    -- check that the rest matches
    let name = conName (undefined :: M1 C c f x)
    guard (n == toLower (cs name))
    M1 <$> genRoutePage ps

  genRoutePage [] = Nothing

  genPageRoute (M1 x) =
    let name = conName (undefined :: M1 C c f x)
    in (toLower $ cs name):(genPageRoute x)

-- Unary constructors
instance GenRoute U1 where
  genRoutePage [] = pure U1
  genRoutePage _ = Nothing
  genPageRoute _ = []


-- Selectors
instance (GenRoute f) => GenRoute (M1 S c f) where
  genRoutePage ps = do
    M1 <$> genRoutePage ps

  genPageRoute (M1 x) = genPageRoute x

-- Sum types
instance (GenRoute a, GenRoute b) => GenRoute (a :+: b) where
  genRoutePage ps = L1 <$> genRoutePage ps <|> R1 <$> genRoutePage ps
  genPageRoute (L1 a) = genPageRoute a
  genPageRoute (R1 a) = genPageRoute a


-- Route Param Types
instance GenRoute (K1 R Integer) where
  genRoutePage = genRouteRead
  genPageRoute (K1 n) = [cs $ show n]

instance GenRoute (K1 R Text) where
  genRoutePage [t] = pure $ K1 t
  genRoutePage _ = Nothing
  genPageRoute (K1 t) = [cs t]

genRouteRead :: Read x => [Text] -> Maybe (K1 R x a)
genRouteRead [t] = do
  K1 <$> readMaybe (cs t)
genRouteRead _ = Nothing



-- instance GenRoute f => GenRoute (M1 C d f) where
--   genRoutePage ps = M1 <$> genRoutePage ps


-- instance RoutePage AppPage where
--   routePage ["counter", n] = do
--     cnt <- readMaybe (unpack n)
--     pure $ Counter cnt
--   routePage ["focus"] = pure Focus
--   routePage ["todos"] = pure Todos
--   routePage ["signup"] = pure Signup
--   routePage ["article", id'] = do
--     pure $ Article id'
--   routePage ["index"] = pure Index
--   routePage _ = Nothing