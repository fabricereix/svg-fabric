{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes where

import qualified Svg.Attributes.Animate as Animate
import qualified Svg.Attributes.Circle as Circle
import qualified Svg.Attributes.G as G
import qualified Svg.Attributes.Path as Path
import qualified Svg.Attributes.Polyline as Polyline
import qualified Svg.Attributes.Rect as Rect
import qualified Svg.Attributes.Style as Style
import qualified Svg.Attributes.Svg as Svg
import qualified Svg.Attributes.Symbol as Symbol
import qualified Svg.Attributes.Text as Text
import qualified Svg.Attributes.Use as Use


all :: String -> [String]
all "animate" =  Animate.all
all "circle" =  Circle.all
all "g" =  G.all
all "path" =  Path.all
all "polyline" =  Polyline.all
all "rect" =  Rect.all
all "style" =  Style.all
all "svg" =  Svg.all
all "symbol" =  Symbol.all
all "text" =  Text.all
all "use" =  Use.all
all name = error $ "element " ++ name ++ " not found"

