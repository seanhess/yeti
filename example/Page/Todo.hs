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

import Wookie
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, STM, modifyTVar)
import Control.Lens (Lens', lens, (+=), (-=), (.=), (^.), makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)
import Control.Monad.State.Lazy (StateT, gets, get)
import Data.Map as Map (lookup, (!?))
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, isInfixOf, toLower)
import Data.Function ((&))
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5



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
  { _todos :: [Todo]
  , _search :: Text
  , _addContent :: Text
  } deriving (Show, Eq)

makeLenses ''Model



params :: Model -> Params
params m = (m ^. search, m ^. addContent)




load :: MonadIO m => TVar [Todo] -> Maybe Params -> m Model
load savedTodos ps = do
  let (s,t) = fromMaybe (("", "")) ps :: Params
  ts <- liftIO $ atomically $
    readTVar savedTodos
  pure $ Model
    { _todos = ts
    , _search = s
    , _addContent = t
    }




data Action
  = AddTodo
  | NewTodoInput Value
  | Delete Text
  | Search Value
  deriving (Show, Read)
instance PageAction Action



update :: MonadIO m => TVar [Todo] -> Action -> StateT Model m ()
update savedTodos (AddTodo) = do
  m <- get
  let new = Todo (m ^. addContent) False
  ts <- liftIO $ atomically $ appendTodo savedTodos new

  search .= ""
  addContent .= ""
  todos .= ts

update savedTodos (Delete t) = do
  ts <- liftIO $ atomically $ deleteTodo savedTodos t
  todos .= ts

update _ (Search (Value s)) = do
  search .= s

update _ (NewTodoInput (Value s)) = do
  addContent .= s




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


view :: Model -> Html ()
view m = div_ $ do
  h3_ "Todos"

  div_ [ id_ "add", style_ "margin:10" ] $ do
    button_ [ onClick AddTodo ] "Add"
    input_ [ name_ "add", value_ (m ^. addContent), onInput (NewTodoInput), onEnter AddTodo ]

  div_ [ id_ "search", style_ "margin:10" ] $ do
    button_ [ onClick Submit, onEnter Submit ] "Search"
    input_ [ name_ "search", value_ (m ^. search), onInput (Search), onEnter Submit ]


  let ts = (m ^. todos) & filter (isSearch (m ^. search))

  div_ $ do
    forM_ ts $ \todo ->
      div_ $ do
        span_ $ button_ [ onClick (Delete (content todo)) ] "X"
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
