module Svg.Playground.Setter.Animate where

import Svg.Playground.Elements
import Svg.Types.Core



fill :: Element -> OneOf1 RemoveFreeze -> Element
fill (Animate _) v = Animate v
fill _            _ = error "Element should be an animate"