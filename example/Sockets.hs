module Sockets where

import Lucid

class SPage model m | model -> m where
  type Msg model :: *
  type Params model :: *

  -- this has to work for ALL m
  load' :: m model
  params' :: model -> Params model
  update' :: Msg model -> model -> m model
  view' :: model -> Html ()
