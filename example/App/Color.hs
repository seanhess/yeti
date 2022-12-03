module App.Color where

import Prelude
import Yeti
import Yeti.UI
import qualified Data.Text as Text
import Data.Text (pack)


-- Example of App Colors
data AppColor
  = Purple
  | PurpleLight
  | Green
  | White
  | Red
  | Gray
  | GrayLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Purple = rgb 168 85 247
  colorValue PurpleLight = rgb 192 132 252
  colorValue Green = Style Hex "00FF00"
  colorValue White = Style Hex "FFF"
  colorValue Red = Style Hex "F00"
  colorValue Gray = Style Hex "DDD"
  colorValue GrayLight = Style Hex "EBEBEB"

  colorName = Text.toLower . pack . show