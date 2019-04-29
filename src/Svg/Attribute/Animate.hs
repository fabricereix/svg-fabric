module Svg.Attribute.Animate where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.Animate as Animate


fill :: Element -> String
fill element = case Animate.fill element of
    OneOf1 value ->  formatRemoveFreeze value


