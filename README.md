# Juniper

Juniper is a reactive server-side rendered web framework for Haskell. 

## Best of both worlds

Remember 1999? Before Flash and SPAs, a web application was nothing more than a PHP page: it would load some data and render it. Any interactivity was achieved by linking to another page or POSTing to another PHP page. Pages were completely independent of each other, and all the chaos was contained in one page.

Today we have React and Elm, which update a view based on a changing state. These modern frameworks are much more interactive. However, we have to write two applications, usually in two languages: a Haskell server and a Javascript app running on the browser. We must design an API to allow the two to communicate.

Juniper is an implementation of Elm running on a Haskell server. State is maintained in the URL, all logic runs on the server, then then views are rendered efficiently via virtual dom on the client.

- *One Application* - There's no need to think about an API. You can render your data models directly. Write all your logic in Haskell
- *Isolated Views* - Each page is an independent application. You can reuse code of course.
- *Virtual DOM* - The rendered HTML is diffed by an Elm application running on the client. Only the parts that have changed are re-rendered

## Examples

### Sample Application

```haskell
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Page.Counter where

import Control.Monad.IO.Class (MonadIO)
import Juniper
import Lucid (Html, toHtml)
import Lucid.Html5

data Model = Model
  { count :: Integer
  } deriving (Show, Read, ToParams)

data Action
  = Increment
  | Decrement
  deriving (Show, Read, PageAction)


load :: MonadIO m => m Model
load = pure $ Model 0

update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ m { count = count m + 1 }
update Decrement m = pure $ m { count = count m - 1 }

view :: Model -> Html ()
view m =
  section_ [ class_ "page" ] $ do
    div_ [ class_ "section" ] $ do
      button_ [ onClick Increment] "Increment"
      button_ [ onClick Decrement] "Decrement"


page :: MonadIO m => Page Model Model Action m
page = simplePage load update view
```

### Signup Form

[https://github.com/seanhess/juniper/blob/main/example/Page/Signup.hs](Code)

### Todos

[https://github.com/seanhess/juniper/blob/main/example/Page/Todo.hs](Code)







