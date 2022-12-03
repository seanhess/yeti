{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Signup where

import Prelude
import Yeti
import Yeti.UI
import App.Color

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock as Time (UTCTime, getCurrentTime)
import Data.Text as Text (Text, length, any)
import Data.Char (isDigit, isAlpha, isPunctuation, isUpper, isLower)
import Data.Maybe (catMaybes)


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



view :: Model -> View Content ()
view m = col (gap 20 . pad 10) $ do
  case m.signup of
    Working v -> workingView m v
    Valid -> validView

workingView :: Model -> Validation -> View Content ()
workingView m v = do

  el (bold) "Choose a username and password"

  field (gap 8 . invalidRed validUser) LabelAbove "Username" $ do
    text_ "Username"
    mapM_ text_ (userErrorMessages v)
    inputText EditUsername m.username (invalidRed validUser)

  field (gap 8 . invalidRed validPass) LabelAbove "Password" $ do
    mapM_ text_ (passErrorMessages v)
    inputText EditPass1 m.pass1 (invalidRed validPass)

  field (gap 8 . invalidRed validPass) LabelAbove "Re-type password" $ do
    inputText EditPass2 m.pass2 (invalidRed validPass)

  button SignUp id "Sign Up"

  where
    invalidRed f = if f v then id else color Red



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


validView :: View Content ()
validView = do
  el (bold) "Thanks for signing up!"


page :: MonadIO m => Page () Model Action m
page = simplePage load update view







-- Simulated Effects ------------------------------

checkIsUsernameUsed :: Applicative m => Username -> m Bool
checkIsUsernameUsed n = do
    pure $ n `elem` existing
  where
    existing :: [Username]
    existing = ["henry", "david", "bob", "alison", "clarice", "stephanie"]


