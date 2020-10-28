{-# LANGUAGE OverloadedStrings #-}
module Wookie.Events where


import Wookie.Page (PageAction(..))
import Lucid.Base (makeAttribute, Attribute)


click :: PageAction action => action -> Attribute
click = makeAttribute "data-click" . showAction

