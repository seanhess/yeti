{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Page.Signup where


import Wookie

import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text as Text (Text, length, any)
import Data.Char (isDigit, isAlpha, isPunctuation, isUpper, isLower)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad.State.Lazy (StateT, get)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5 hiding (onclick_)

-- TODO serialize fragments. Map fragments to specific sub-components. Route Actions to those components.
-- TODO params inside a model instead of mapping. Serialize to JSON now that they are hidden?


data Action
  = EditUsername Value
  | EditPass1 Value
  | EditPass2 Value
  | SignUp
  deriving (Show, Read)
instance PageAction Action


data Params = Params
  { _message :: Text
  , _username :: Text
  , _pass1 :: Text
  , _pass2 :: Text
  } deriving (Show, Eq, Read, ToParams)

makeLenses ''Params


type Username = Text


data Signup
  = Working Validation
  | Valid
  deriving (Show, Eq)

data Validation = Validation
  { passwordsDoNotMatch :: Bool
  , passwordInvalid :: Bool
  , usernameIsTaken :: Bool
  , usernameTooShort :: Bool
  } deriving (Show, Eq)

data Model = Model
  { _params :: Params
  , _timestamp :: UTCTime
  , _signup :: Signup
  } deriving (Show, Eq)

makeLenses ''Model

passwordsMatch :: Text -> Text -> Bool
passwordsMatch = (==)

passwordValid :: Text -> Bool
passwordValid p =
     Text.any (isDigit) p
  && Text.any (isAlpha) p
  && Text.any (isUpper) p
  && Text.any (isLower) p
  && Text.any (isPunctuation) p


usernameShort :: Text -> Bool
usernameShort t = Text.length t < 5

validUser :: Validation -> Bool
validUser v = (not $ usernameIsTaken v) && (not $ usernameTooShort v)

validPass :: Validation -> Bool
validPass v = (not $ passwordInvalid v) && (not $ passwordsDoNotMatch v)


load :: MonadIO m => Maybe Params -> m Model
load mps = do
  let p = fromMaybe (Params "hello" "" "" "") mps
  t <- liftIO $ Time.getCurrentTime
  let v = Validation False False False False
  pure $ Model p t (Working v)



update :: MonadIO m => Action -> StateT Model m ()
update (EditUsername (Value t)) = (params.username) .= t
update (EditPass1 (Value t)) = (params.pass1) .= t
update (EditPass2 (Value t)) = (params.pass2) .= t
update SignUp = do
  m <- get
  v <- validate (m ^. params . username) ( m ^. params . pass1) (m ^.params.pass2)
  signup .= if isValid v then Valid else Working v


validate :: Monad m => Text -> Text -> Text -> m Validation
validate u p1 p2 = do
  used <- checkIsUsernameUsed u
  pure $ Validation
    { passwordsDoNotMatch = not (passwordsMatch p1 p2)
    , usernameTooShort = usernameShort u
    , usernameIsTaken = used
    , passwordInvalid = not $ passwordValid p1
    }

isValid :: Validation -> Bool
isValid v = not (passwordsDoNotMatch v || usernameTooShort v || usernameIsTaken v || passwordInvalid v)



-- we want to validate, and give the errors here
--

view :: Model -> Html ()
view m = section_ $ do
  case m ^. signup of
    Working v -> workingView m v
    Valid -> validView

workingView :: Model -> Validation -> Html ()
workingView m v = do
  p_ "Choose a username and password"

  div_ [ class_ $ if (validUser v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "username" ] "Username"
    div_ $ do
      input_ [ name_ "username", type_ "text", value_ (m ^. params . username), onInput (EditUsername) ]
      mapM_ (span_ [ class_ "message" ] . toHtml) (userErrorMessages v)

  div_ [ class_ $ if (validPass v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "password1" ] "Password"
    div_ $ do
      input_ [ name_ "password1", type_ "password", value_ (m ^. params . pass1), onInput (EditPass1) ]
      mapM_ (span_ [ class_ "message" ] . toHtml) (passErrorMessages v)

  div_ [ class_ $ if (validPass v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "password2" ] "Re-type Password"
    div_ $ input_ [ name_ "password2", type_ "password", value_ (m ^. params . pass2), onInput (EditPass2) ]

  div_ [ class_ "row" ] $
    button_ [ onClick SignUp ] "Sign Up"


userErrorMessages :: Validation -> [Text]
userErrorMessages v = catMaybes
  [ if (usernameTooShort v) then Just "Username is too short" else Nothing
  , if (usernameIsTaken v) then Just "Username is taken" else Nothing
  ]

passErrorMessages :: Validation -> [Text]
passErrorMessages v = catMaybes
  [ if (passwordInvalid v) then Just "Password must have upper and lower case letters, a number, and punctuation" else Nothing
  , if (passwordsDoNotMatch v) then Just "Passwords do not match" else Nothing
  ]


validView :: Html ()
validView = do
  p_ "Thanks for signing up!"


page :: MonadIO m => Page Params Model Action m
page = Page _params load update view


-- Simulated Effects ------------------------------

checkIsUsernameUsed :: Applicative m => Username -> m Bool
checkIsUsernameUsed n = do
    pure $ n `elem` existing
  where
    existing = ["henry", "david", "bob", "alison", "clarice", "stephanie"]


