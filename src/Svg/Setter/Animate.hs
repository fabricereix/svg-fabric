module Svg.Setter.Animate where
import Svg.Elements
import Svg.Types.Core


fill :: Element -> OneOf1 RemoveFreeze -> Element
fill (Animate a0 _) v  = (Animate a0 v)
fill _ _  = error "Element should be Animate!"

