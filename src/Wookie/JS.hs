{-# LANGUAGE TemplateHaskell #-}

module Wookie.JS where
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- | Embed built javascript into file via Data.FileEmbed. Must be recompiled via node to work
build :: ByteString
build = $(embedFile "dist/build.js")
