module Svg.Getter.Animate where
import Svg.Elements
import Svg.Types.Core


fill :: Element -> OneOf1 RemoveFreeze
fill (Animate _ v) = v
fill _  = error "Element should be Animate!"

