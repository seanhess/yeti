{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Page.Todo where

import Wookie.Page
import Wookie.Events (click, FormData(..), Value(..), defaultValue, onUpdate, Apply(..), onEnter)

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, STM, modifyTVar)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)
import Data.Map as Map (lookup, (!?))
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, isInfixOf, toLower)
import Data.Function ((&))
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5 hiding (onclick_)



-- 



-- TESTS
-- TODO Pre-render the first page load - complete
-- TODO two counters
-- TODO serialize fragments. Map fragments to specific sub-components. Route Actions to those components.
-- TODO VDOM rendering





data Todo = Todo
  { content :: Text
  , completed :: Bool
  } deriving (Show, Eq)


type Params = (Text, Text)

data Model = Model
  { todos :: [Todo]
  , search :: Text
  , addContent :: Text
  } deriving (Show, Eq)



params :: Model -> Params
params m = (search m, addContent m)




load :: MonadIO m => TVar [Todo] -> Maybe Params -> m Model
load savedTodos ps = do
  let (s,t) = fromMaybe (("", "")) ps :: Params
  ts <- liftIO $ atomically $
    readTVar savedTodos
  pure $ Model
    { todos = ts
    , search = s
    , addContent = t
    }




data Action
  = AddTodo
  | NewTodoInput Value
  | Delete Text
  | Search Value
  deriving (Show, Read)
instance PageAction Action



update :: MonadIO m => TVar [Todo] -> Action -> Model -> m Model
update savedTodos (AddTodo) m = do
  let new = Todo (addContent m) False
  ts <- liftIO $ atomically $ appendTodo savedTodos new

  pure $ m { search = "", addContent = "", todos = ts }

update savedTodos (Delete t) m = do
  ts <- liftIO $ atomically $ deleteTodo savedTodos t
  pure $ m { todos = ts }

update _ (Search (Value s)) m = do
  pure $ m { search = s }

update _ (NewTodoInput (Value s)) m = do
  pure $ m { addContent = s }




appendTodo :: TVar [Todo] -> Todo -> STM [Todo]
appendTodo savedTodos t = do
  ts <- readTVar savedTodos
  let ts' = ts <> [t]
  writeTVar savedTodos ts'
  pure ts'


deleteTodo :: TVar [Todo] -> Text -> STM [Todo]
deleteTodo savedTodos t = do
  ts <- readTVar savedTodos
  let ts' = filter (\(Todo t' _) -> t /= t') ts
  writeTVar savedTodos ts'
  pure ts'



  -- modifyTVar savedTodos (\ts -> ts <> [t])


-- TODO Fix this apply thing! It's not obvious that you have to do that
-- Or rather, make onUpdate be super loud, like it's obviously different from click, etc
view :: Model -> Html ()
view m = div_ $ do
  h3_ "Todos"

  div_ [ id_ "add", style_ "margin:10" ] $ do
    button_ [ click AddTodo ] "Add"
    input_ [ name_ "add", value_ (addContent m), onUpdate (NewTodoInput), onEnter AddTodo ]

  div_ [ id_ "search", style_ "margin:10" ] $ do
    button_ [ click Apply, onEnter Apply ] "Search"
    input_ [ name_ "search", value_ (search m), onUpdate (Search), onEnter Apply ]


  let ts = (todos m) & filter (isSearch (search m))

  div_ $ do
    forM_ ts $ \todo ->
      div_ $ do
        span_ $ button_ [ click (Delete (content todo)) ] "X"
        -- this is going to be a button
        span_ "✓"
        span_ "☑"
        span_ "☐"
        span_ $ toHtml (content todo)



isSearch :: Text -> Todo -> Bool
isSearch "" _ = True
isSearch t (Todo t' _) = Text.isInfixOf (Text.toLower t) (Text.toLower t')





page :: MonadIO m => TVar [Todo] -> Page Params Model Action m
page savedTodos =
  Page
    params
    (load savedTodos)
    (update savedTodos)
    view
