{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Todo where

import Juniper
import Juniper.Prelude
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, STM, modifyTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)
import Data.Map as Map (lookup, (!?))
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, isInfixOf, toLower)
import Data.Function ((&))
import Lucid (Html, toHtml, toHtmlRaw, renderBS)
import Lucid.Html5

-- the only parameter is the search text
data Params = Params
  { search :: Text
  } deriving (Generic, ToJSON, FromJSON, ToParams)

data Model = Model
  { todos :: [Todo]
  , search :: Text
  , addContent :: Text
  } deriving (Generic, ToJSON, FromJSON, ToParams)

instance HasParams Model Params where
  toParams m = Params m.search
  defParams = Params ""

-- each one has to mention all of them!
-- instance ToParams Model Params where
--   params :: Model -> Params
--   params = (.search)

data Todo = Todo
  { content :: Text
  , completed :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data Action
  = AddTodo
  | NewTodoInput Value
  | Delete Text
  | Search Value
  deriving (Show, Read, PageAction)





-- only calls load if the model is missing
load :: MonadIO m => TVar [Todo] -> Params -> m Model
load savedTodos p = do
  ts <- liftIO $ atomically $ readTVar savedTodos
  pure $ Model
    { todos = ts
    , search = p.search
    , addContent = ""
    }






update :: MonadIO m => TVar [Todo] -> Action -> Model -> m Model
update savedTodos (AddTodo) m = do
  let new = Todo (m.addContent) False
  ts <- liftIO $ atomically $ appendTodo savedTodos new
  pure $ m
    { search = ""
    , addContent = ""
    , todos = ts
    }

update savedTodos (Delete t) m = do
  ts <- liftIO $ atomically $ deleteTodo savedTodos t
  pure $ m { todos = ts }

update _ (Search (Value s)) m = do
  pure $ (m :: Model) { search = s }

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




view :: Model -> Html ()
view m = div_ $ do
  h3_ "Todos"

  div_ [ id_ "add", style_ "margin:10" ] $ do
    button_ [ onClick AddTodo ] "Add"
    input_ [ name_ "add", value_ (m.addContent), onInput (NewTodoInput), onEnter AddTodo ]

  div_ [ id_ "search", style_ "margin:10" ] $ do
    button_ [ onClick Submit, onEnter Submit ] "Search"
    input_ [ name_ "search", value_ (m.search), onInput (Search), onEnter Submit ]


  let ts = m.todos & filter (isSearch m.search)

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
page savedTodos = Page toParams (load savedTodos) (update savedTodos) view
