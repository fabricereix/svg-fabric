module Svg.Attribute where

import Svg.Elements
import Data.Maybe

import qualified Svg.Attribute.Animate as Animate
import qualified Svg.Attribute.Circle as Circle
import qualified Svg.Attribute.Rect as Rect
import qualified Svg.Attribute.Svg as Svg


attributes :: Element -> [(String,String)]
attributes element@(Animate _ _) =
     case Animate.fill element of "remove" -> []; value -> [("fill", value)]

attributes element@(Circle _ _ _ _ _ _) =
     case Circle.cx element of "0" -> []; value -> [("cx", value)]
  ++ case Circle.cy element of "0" -> []; value -> [("cy", value)]
  ++ case Circle.r element of "0" -> []; value -> [("r", value)]
  ++ map (\v->("pathLength",v)) (maybeToList $ Circle.pathLength element)
  ++ map (\v->("fill",v)) (maybeToList $ Circle.fill element)

attributes element@(Rect _ _ _ _ _ _) =
     case Rect.x element of "0" -> []; value -> [("x", value)]
  ++ case Rect.y element of "0" -> []; value -> [("y", value)]
  ++ case Rect.width element of "auto" -> []; value -> [("width", value)]
  ++ case Rect.height element of "auto" -> []; value -> [("height", value)]
  ++ map (\v->("fill",v)) (maybeToList $ Rect.fill element)

attributes element@(Svg _ _ _ _) =
     case Svg.width element of "auto" -> []; value -> [("width", value)]
  ++ case Svg.height element of "auto" -> []; value -> [("height", value)]
  ++ map (\v->("viewport",v)) (maybeToList $ Svg.viewport element)


