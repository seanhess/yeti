{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module Yeti.View.Tailwind.Types where

import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Char (isLower, isUpper)
import Prelude hiding ((-))
import Text.Casing as Casing (kebab)
import qualified Data.Text as Text

newtype Seg a = Seg { fromSeg :: Text }
  deriving newtype (Eq, IsString, Semigroup)
  deriving Show

class Segment a where
  seg :: a -> Seg b
  default seg :: Show a => a -> Seg b
  seg = segHyphens

instance Segment () where
  seg _ = ""

class Option k a where
  option :: a -> Seg k

  default option :: Segment a => a -> Seg k
  option = seg

segHyphens :: Show a => a -> Seg b
segHyphens a = Seg $ hyphenate $ show a

hyphenate :: String -> Text
hyphenate = Text.toLower . pack . Casing.kebab

-- drops a prefix caps etc
dropPrefix :: String -> String
dropPrefix = dropWhile isLower . dropWhile isUpper

-- drop until second cap
segPrefix :: Show a => a -> Seg b
segPrefix a = Seg $ hyphenate $ dropPrefix $ show a


(-) :: Seg a -> Seg b -> Seg a
a - "" = a
(Seg a) - (Seg b) = Seg $ a <> "-" <> b

newtype Class = Class { fromClass :: Text }
  deriving newtype (IsString)
  deriving (Show, Eq)


-- * Utilties

cls :: Seg a -> [Class]
cls (Seg t) = [Class t]

