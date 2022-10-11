{-# LANGUAGE OverloadedLists #-}
module Yeti.View
  ( module Yeti.View.Types
  , test
  ) where

import Yeti.Prelude
import Yeti.View.Types
import Data.Aeson


test :: IO ()
test = do
  putStrLn "TEST"
  print doc
  print content

  -- -- woot!
  let out = encode $ vdom $ content
  putStrLn $ cs out
  -- print (decode out :: Maybe VDOM)

  where
    doc = document "my title" content

    hello = 
      text (border) "Hello World"
      -- tag "div" [attribute "key" "value"] $ text "hello world"

    myAtt = setAttribute "my-attribute"

    green = addClass ["green-text"]

    content = do
      text (myAtt "value") "hello world"

      node (border) $ do
        -- we would never want to put multiple text nodes as siblings. Or at least very rarely
        -- we may want to span them, but whatever

        -- do we want this instead of text, row, col, etc?
        node (green) ("one" :: View Content ()) 
        text (addClass ["check", "asdf"] . addClass ["woot woot"]) "two"
        text (green) "three"


border :: Att a
border = addClass ["border-1"]

