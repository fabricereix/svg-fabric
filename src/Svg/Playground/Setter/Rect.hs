module Svg.Playground.Setter.Rect where

import Svg.Playground.Elements
import Svg.Types.Core

x :: Element ->OneOf2 Length Percentage -> Element
x (Rect _ a2) v = Rect v a2
x _           _ = error "Element should be a rect"

fill :: Element -> Maybe (OneOf1 Paint) -> Element
fill (Rect a1 _) v = Rect a1 v
fill _            _ = error "Element should be a rect"