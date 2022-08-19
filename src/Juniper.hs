module Juniper
 ( Page(Page)
 , Encode(..)
 , LiveAction, LiveModel
 , simplePage
 , Submit(..)
 , ToParams(..), ToParam(..)
 , Value(..)
 , page
 , handle
 , Render(..)
 , onInput, onClick, onEnter --, onBlur
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




