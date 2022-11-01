{-# LANGUAGE OverloadedLists #-}
module Yeti.View
  ( module Yeti.View.Types
  ) where

-- import Yeti.Prelude
import Yeti.View.Types

-- To import UI, you should have the following
-- import Yeti
-- import Yeti.View.UI

-- need to re-export tailwind stuff somewhere
-- import Yeti.View.UI
-- import Yeti.View.Tailwind.Values
-- import Data.Aeson


-- test :: IO ()
-- test = do
--   putStrLn "TEST"
--   print doc
--   print (body :: View Content ())

--   -- -- woot!
--   let out = encode $ vdom body
--   putStrLn $ cs out
--   let back = decode out :: Maybe VDOM
--   print back
--   -- print (decode out :: Maybe VDOM)

--   where
--     doc = document "my title" body

--     hello = 
--       txt (border B1) "Hello World"
--       -- tag "div" [attribute "key" "value"] $ text "hello world"

--     myAtt = setAttribute "my-attribute"

--     green = addClass ["green-text"]

--     body = col (p S0) $ do
--       txt (myAtt "value") "hello world"

--       col (border B2) $ do
--         -- we would never want to put multiple text nodes as siblings. Or at least very rarely
--         -- we may want to span them, but whatever

--         -- do we want this instead of text, row, col, etc?
--         el (green) ("one" :: View Content ()) 
--         txt (addClass ["check", "asdf"] . addClass ["woot woot"]) "two"
--         txt (green) "three"



