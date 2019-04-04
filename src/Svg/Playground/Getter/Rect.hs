module Svg.Playground.Getter.Rect where

import Svg.Playground.Elements
import Svg.Types.Core


x :: Element ->OneOf2 Length Percentage
x (Rect v _) = v
x _          = error "Element should be a rect"


fill :: Element -> Maybe (OneOf1 Paint)
fill (Rect _ v) = v
fill _          = error "Element should be a rect"