{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Page.Todo where

import Yeti
import Yeti.UI
import Prelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, STM)
import Data.Text as Text (Text, isInfixOf, toLower)
import Data.Function ((&))
import App.Color


data Model = Model
  { todos :: [Todo]
  , search :: Text
  , searchCurrent :: Text
  , addContent :: Text
  , addCategory :: Category
  } deriving (Show, Generic, LiveModel, FromJSON)

-- the only parameter is the search text
data Params = Params
  { _search :: Text
  } deriving (Generic, ToParams)

params :: Model -> Params
params m = Params m.search


data Category
  = Errand
  | Work
  | Home
  | Personal
  deriving (Show, Generic, ToJSON, FromJSON)
instance Input Category where
  empty = Errand

data Todo = Todo
  { content :: Text
  , category :: Category
  , completed :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)






data Action
  = AddTodo
  | Test
  | NewTodoInput Text
  | NewTodoCategory Category
  | SetCompleted Text Bool
  | Delete Text
  | SearchFilter Text
  | SearchSave
  deriving (Show, Generic, LiveAction)





-- you have to load even if no params were specified
load :: MonadIO m => TVar [Todo] -> Maybe Params -> m Model
load savedTodos mps = do
  -- if params search is set, set ours
  let (Params src) = fromMaybe (Params "") $ mps
  ts <- liftIO $ atomically $ readTVar savedTodos
  pure $ Model
    { todos = ts
    , search = src
    , searchCurrent = src
    , addContent = ""
    , addCategory = Errand
    }






update :: MonadIO m => TVar [Todo] -> Action -> Model -> m Model
update todos (AddTodo) m = do
  let new = Todo (m.addContent) (m.addCategory) False
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

update _ (NewTodoCategory c) m = do
  pure $ m { addCategory = c }

update _ (NewTodoInput t) m = do
  pure $ m { addContent = t }

update _ Test m = do
  pure m

update _ (SearchFilter s) m = do
  pure $ (m :: Model) { searchCurrent = s }

update _ (SearchSave) m = do
  pure $ (m :: Model) { search = m.searchCurrent }



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



view :: Model -> View Content ()
view m = col (gap 10) $ do
  el bold "Todos"

  field (gap 8) LabelLeft "Todo:" $ do
    inputText NewTodoInput m.addContent (onEnter AddTodo)

  field (gap 8) LabelLeft "Category:" $ do
    dropdown NewTodoCategory (pad 2) (cs . show) (text_ . cs . show)
      [Errand, Home, Work, Personal]

  button AddTodo (pad 8 . bg Gray . hover|:bg GrayLight) "Add Todo"

  field (gap 8) LabelAbove "Search" $ do
    inputSearch SearchFilter m.search (onEnter SearchSave)

  button SearchSave id "Search"


  let todos = m.todos & filter (isSearch m.searchCurrent)

  col (gap 8) $ do
    forM_ todos $ \todo ->
      row (gap 8) $ do
        button (Delete todo.content) id "x"
        checkbox (SetCompleted todo.content) (completed todo)
        text_ $ todo.content


checkbox :: LiveAction action => (Bool -> action) -> Bool -> View Content ()
checkbox act chk = do
  button (act (not chk)) id $ do
    if chk
      then "▣"
      else "▢"


isSearch :: Text -> Todo -> Bool
isSearch "" _ = True
isSearch t (Todo t' _ _) = Text.isInfixOf (Text.toLower t) (Text.toLower t')



page :: MonadIO m => TVar [Todo] -> Page Params Model Action m
page savedTodos = Page params (load savedTodos) (update savedTodos) view
