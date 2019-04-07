module Svg.Playground.Dump.Rect where

import Svg.Playground.Elements
import qualified Svg.Playground.Getter.Rect as Rect
import Svg.Types.Format

x :: Element -> Maybe (String,String)
x element = if Rect.x element == Rect.x defaultRect then Nothing
     else case Rect.x element of
       OneOf2 value -> Just ("x", formatLength value)
       TwoOf2 value -> Just ("x", formatPercentage value)

fill :: Element -> Maybe (String,String)
fill element = case Rect.fill element of
     Nothing -> Nothing
     Just (OneOf1 value) -> Just ("fill", formatPaint value)


