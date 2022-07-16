module Juniper.JS where

import Juniper.Prelude
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

scripts :: ByteString
scripts = build <> "\n" <> run

-- | Embed built javascript into file via Data.FileEmbed. Must be recompiled via node to work
build :: ByteString
build = $(embedFile "edom/build.min.js")

run :: ByteString
run = $(embedFile "edom/run.js")
