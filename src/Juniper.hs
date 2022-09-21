module Juniper
 ( Page(Page)
 , LiveModel(encodeModel, decodeModel)
 , LiveAction, encodeAction, encodeAction1, decodeAction, Input(..)
 , simplePage
 , Submit(..)
 , ToParams(..), ToParam(..)
--  , Value(..)
 , page
 , handle, simpleDocument
 , Render(..)
 , onInput, onClick, onEnter, onSelect, onValue, on
 , scripts
 , Html
 , FromJSON, ToJSON, Generic
 , module Juniper.Component
 ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Juniper.Runtime
import Juniper.Events
import Juniper.Params
import Juniper.Web
import Juniper.Component
import Juniper.JS (scripts)
import Lucid (Html)
import Juniper.Encode




