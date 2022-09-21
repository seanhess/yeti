module Sockets where

import Lucid

-- class SPage model where
--   type Msg model :: *
--   type Params model :: *

--   -- this has to work for ALL m
--   load' :: m model
--   params' :: model -> Params model
--   update' :: Msg model -> model -> m model
--   view' :: model -> Html ()

-- -- does it have to be a member of params?
class SParams model params where
  params' :: model -> params

class SView model where
  view' :: model -> Html ()

class SUpdate model m where
  type Msg model :: *
  load' :: m model
  update' :: Msg model -> model -> m model

-- class SLiveAction model action m where
--   update' :: action -> model -> m model


-- could we make an app, and have it always forward things?
-- no, we don't want it to worry about whether it serializes correctly
