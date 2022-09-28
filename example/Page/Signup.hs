{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Signup where

import Prelude
import Yeti

import Data.Aeson (FromJSON, ToJSON)
import Data.String.Conversions (cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text as Text (Text, length, any)
import Data.Char (isDigit, isAlpha, isPunctuation, isUpper, isLower)
import Data.Maybe (fromMaybe, catMaybes)
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5 hiding (onclick_)


-- data Params = Params
--   { message :: Text
--   , username :: Text
--   , pass1 :: Text
--   , pass2 :: Text
--   } deriving (Show, Eq, Generic, ToJSON, FromJSON, ToParams)

type Username = Text

data Signup
  = Working Validation
  | Valid
  deriving (Show, Generic, ToJSON, FromJSON)

data Validation = Validation
  { passwordsDoNotMatch :: Bool
  , passwordInvalid :: Bool
  , usernameIsTaken :: Bool
  , usernameTooShort :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)


-- EXAMPLE of params inside a model. It keeps them easy to separate, no?
data Model = Model
  { username :: Text
  , pass1 :: Text
  , pass2 :: Text
  , timestamp :: UTCTime
  , signup :: Signup
  } deriving (Show, Generic, LiveModel)


data Action
  = EditUsername Text
  | EditPass1 Text
  | EditPass2 Text
  | SignUp
  deriving (Show, Generic, LiveAction)


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


load :: MonadIO m => m Model
load = do
  t <- liftIO $ Time.getCurrentTime
  let v = Validation False False False False
  pure $ Model "" "" "" t (Working v)



update :: MonadIO m => Action -> Model -> m Model
update (EditUsername t) m =
  pure $ m { username = t }
update (EditPass1 t) m =
  pure $ m { pass1 = t }
update (EditPass2 t) m =
  pure $ m { pass2 = t }
update SignUp m = do
  v <- validate m.username m.pass1 m.pass2
  pure $ m { signup = if isValid v then Valid else Working v }


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
  case m.signup of
    Working v -> workingView m v
    Valid -> validView

workingView :: Model -> Validation -> Html ()
workingView m v = do
  p_ "Choose a username and password"

  div_ [ class_ $ if (validUser v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "username" ] "Username"
    div_ $ do
      input' [ name_ "username", type_ "text", value_ (m.username), onInput (EditUsername) ] ""
      mapM_ (span_ [ class_ "message" ] . toHtml) (userErrorMessages v)

  div_ [ class_ $ if (validPass v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "password1" ] "Password"
    div_ $ do
      input' [ name_ "password1", type_ "password", value_ (m.pass1), onInput (EditPass1) ] ""
      mapM_ (span_ [ class_ "message" ] . toHtml) (passErrorMessages v)

  div_ [ class_ $ if (validPass v) then "section" else "section error" ] $ do
    div_ $ label_ [ for_ "password2" ] "Re-type Password"
    div_ $ input' [ name_ "password2", type_ "password", value_ (m.pass2), onInput (EditPass2) ] ""

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


page :: MonadIO m => Page () Model Action m
page = simplePage load update view







-- Simulated Effects ------------------------------

checkIsUsernameUsed :: Applicative m => Username -> m Bool
checkIsUsernameUsed n = do
    pure $ n `elem` existing
  where
    existing :: [Username]
    existing = ["henry", "david", "bob", "alison", "clarice", "stephanie"]


