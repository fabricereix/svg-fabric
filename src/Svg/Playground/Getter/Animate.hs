module Svg.Playground.Getter.Animate where

import Svg.Playground.Elements
import Svg.Types.Core




fill :: Element -> OneOf1 RemoveFreeze
fill (Animate v) = v
fill _          = error "Element should be an animate"