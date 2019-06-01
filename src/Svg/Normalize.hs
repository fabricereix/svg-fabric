{-# LANGUAGE OverloadedStrings     #-}
module Svg.Normalize where

import qualified Svg.Normalize.Animate as Animate
import qualified Svg.Normalize.Circle as Circle
import qualified Svg.Normalize.G as G
import qualified Svg.Normalize.Path as Path
import qualified Svg.Normalize.Polyline as Polyline
import qualified Svg.Normalize.Rect as Rect
import qualified Svg.Normalize.Style as Style
import qualified Svg.Normalize.Svg as Svg
import qualified Svg.Normalize.Symbol as Symbol
import qualified Svg.Normalize.Text as Text
import qualified Svg.Normalize.Use as Use


normalize :: String -> String -> String -> Either String String
normalize "animate" name value = Animate.normalize name value
normalize "circle" name value = Circle.normalize name value
normalize "g" name value = G.normalize name value
normalize "path" name value = Path.normalize name value
normalize "polyline" name value = Polyline.normalize name value
normalize "rect" name value = Rect.normalize name value
normalize "style" name value = Style.normalize name value
normalize "svg" name value = Svg.normalize name value
normalize "symbol" name value = Symbol.normalize name value
normalize "text" name value = Text.normalize name value
normalize "use" name value = Use.normalize name value
normalize name _ _ = error $ "element " ++ name ++ " not found"


