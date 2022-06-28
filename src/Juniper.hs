module Juniper
 ( Page(Page)
 , simplePage
 , PageAction
 , Submit(..)
 , ToParams
 , ToState
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
import Juniper.Page
import Juniper.Events
import Juniper.Params
import Juniper.State
import Juniper.Web
import Juniper.JS (scripts)
import Lucid (Html)




