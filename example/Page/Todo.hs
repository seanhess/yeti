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


data Model = Model
  { todos :: [Todo]
  , count :: Int
  , search :: Text
  , addContent :: Text
  } deriving (Read, Show, ToState)

-- the only parameter is the search text
data Params = Params
  { search :: Text
  , count :: Int
  } deriving (Generic, ToParams)

toParams :: Model -> Params
toParams m = Params m.search m.count


data Category
  = Errand
  | Work
  | Home
  | Personal
  deriving (Show, Read, Eq)

data Todo = Todo
  { content :: Text
  , category :: Category
  , completed :: Bool
  } deriving (Show, Read)






data Action
  = AddTodo
  | NewTodoInput Value
  | Completed Text Bool
  | Delete Text
  | Search Value
  deriving (Show, Read, PageAction)





-- you have to load even if no params were specified
load :: MonadIO m => TVar [Todo] -> Maybe Params -> m Model
load savedTodos mps = do
  let (Params src cnt) = fromMaybe (Params "" 0) $ mps
  ts <- liftIO $ atomically $ readTVar savedTodos
  pure $ Model
    { todos = ts
    , count = cnt
    , search = src
    , addContent = ""
    }






update :: MonadIO m => TVar [Todo] -> Action -> Model -> m Model
update todos (AddTodo) m = do
  let new = Todo (m.addContent) Errand False
  ts <- liftIO $ atomically $ updateTodos todos $ \ts -> ts <> [new]
  pure $ m
    { search = ""
    , addContent = ""
    , todos = ts
    }

update todos (Delete t) m = do
  ts <- liftIO $ atomically $ updateTodos todos remove
  pure $ m { todos = ts }
  where
    remove = filter (\(Todo t' _ _) -> t /= t')

update todos (Completed ct c) m = do
  ts <- liftIO $ atomically $ updateTodos todos (modify ct complete)
  pure $ m { todos = ts }
  where
    complete t = t { completed = c }

update _ (Search (Value s)) m = do
  pure $ (m :: Model) { search = s }

update _ (NewTodoInput (Value s)) m = do
  pure $ m { addContent = s, count = m.count + 1 }



modify :: Text -> (Todo -> Todo) -> [Todo] -> [Todo]
modify ct up =
  map checkTodo
  where
    checkTodo t =
      if (content t) == ct
        then up t
        else t

updateTodos :: TVar [Todo] -> ([Todo] -> [Todo]) -> STM [Todo]
updateTodos saved up = do
  ts <- readTVar saved
  let ts' = up ts
  writeTVar saved ts'
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

  div_ [ class_ "col g16"] $ do
    forM_ ts $ \todo ->
      div_ [ class_ "row g8" ] $ do
        button_ [ onClick (Delete (content todo)) ] "X"
        checkButton (Completed (content todo)) (completed todo) $
          div_ $ toHtml (content todo)


checkButton :: PageAction action => (Bool -> action) -> Bool -> Html () -> Html ()
checkButton act chk ct =
  button_ [ class_ "row g4", onClick $ act $ not chk ] $ do
    span_ $ if (chk) then "☑" else "☐"
    span_ ct


isSearch :: Text -> Todo -> Bool
isSearch "" _ = True
isSearch t (Todo t' _ _) = Text.isInfixOf (Text.toLower t) (Text.toLower t')





page :: MonadIO m => TVar [Todo] -> Page Params Model Action m
page savedTodos = Page toParams (load savedTodos) (update savedTodos) view
