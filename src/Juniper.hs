module Juniper
 ( Page(Page)
 , Encode, encode, decode, encode1
 , LiveAction, LiveModel, Value(..)
 , simplePage
 , Submit(..)
 , ToParams(..), ToParam(..)
--  , Value(..)
 , page
 , handle
 , Render(..)
 , onInput, onClick, onEnter, onSelect --, onBlur
 , scripts
 , Html
 , FromJSON, ToJSON, Generic
 ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Juniper.Runtime
import Juniper.Events
import Juniper.Params
import Juniper.Web
import Juniper.JS (scripts)
import Lucid (Html)




