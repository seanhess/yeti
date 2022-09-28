module Yeti.Embed where

import Yeti.Prelude
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

javascript :: ByteString
javascript = liveJS

liveJS :: ByteString
liveJS = $(embedFile "dist/main.js")
