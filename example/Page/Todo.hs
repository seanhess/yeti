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
  , search :: Text
  , addContent :: Text
  , addCategory :: Category
  } deriving (Read, Show, Encode LiveModel)

-- the only parameter is the search text
data Params = Params
  { search :: Text
  } deriving (Generic, ToParams)

params :: Model -> Params
params m = Params m.search


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
  | NewTodoInput Int Text
  | NewTodoCategory Category
  | SetCompleted Text Bool
  | Delete Text
  | Search Text
  deriving (Show, Read, Encode LiveAction)





-- you have to load even if no params were specified
load :: MonadIO m => TVar [Todo] -> Maybe Params -> m Model
load savedTodos mps = do
  let (Params src) = fromMaybe (Params "") $ mps
  ts <- liftIO $ atomically $ readTVar savedTodos
  pure $ Model
    { todos = ts
    , search = src
    , addContent = ""
    , addCategory = Errand
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
  where remove = filter (\(Todo t' _ _) -> t /= t')

update todos (SetCompleted ct c) m = do
  ts <- liftIO $ atomically $ updateTodos todos (modify ct complete)
  pure $ m { todos = ts }
  where complete t = t { completed = c }

update todos (NewTodoCategory c) m = do
  pure $ m { addCategory = c }

update todos (NewTodoInput _ t) m = do
  pure $ m { addContent = t }

update _ (Search s) m = do
  pure $ (m :: Model) { search = s }



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
view m = div_ [] $ do
  h3_ "Todos"

  div_ [ class_ "col g8"] $ do

    div_ [ id_ "add" ] $ do
      button_ [ onClick AddTodo ] "Add"
      input_ [ name_ "add", value_ (m.addContent), onInput (NewTodoInput 3), onEnter AddTodo ]
      dropdown NewTodoCategory (\v -> toHtml (cs $ show v :: Text)) [Errand, Home, Work, Personal]

    div_ [ id_ "search" ] $ do
      button_ [ onClick Submit, onEnter Submit ] "Search"
      input_ [ name_ "search", value_ (m.search), onInput (Search), onEnter Submit ]


    let ts = m.todos & filter (isSearch m.search)

    div_ [ class_ "col g8"] $ do
      forM_ ts $ \todo ->
        div_ [ class_ "row g8" ] $ do
          button_ [ onClick (Delete (content todo)) ] "X"
          checkButton (SetCompleted (content todo)) (completed todo) $
            div_ $ toHtml (content todo)


checkButton :: Encode LiveAction action => (Bool -> action) -> Bool -> Html () -> Html ()
checkButton act chk ct =
  button_ [ class_ "row g4", onClick $ act $ not chk ] $ do
    span_ $ if (chk) then "☑" else "☐"
    span_ ct


isSearch :: Text -> Todo -> Bool
isSearch "" _ = True
isSearch t (Todo t' _ _) = Text.isInfixOf (Text.toLower t) (Text.toLower t')


-- we are going to run into similar problems. We have to encode things into values
dropdown :: (Encode LiveAction action, Show val) => (val -> action) -> (val -> Html ()) -> [val] -> Html ()
dropdown act opt vals =
  select_ [ id_ "test" ] $
    mapM_ option vals
  where
    option v = option_ [id_ (cs (show v))] (opt v)



page :: MonadIO m => TVar [Todo] -> Page Params Model Action m
page savedTodos = Page params (load savedTodos) (update savedTodos) view
