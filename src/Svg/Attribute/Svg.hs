module Svg.Attribute.Svg where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.Svg as Svg


width :: Element -> String
width element = case Svg.width element of
    OneOf3 value ->  formatAuto value
    TwoOf3 value ->  formatLength value
    ThreeOf3 value ->  formatPercentage value


height :: Element -> String
height element = case Svg.height element of
    OneOf3 value ->  formatAuto value
    TwoOf3 value ->  formatLength value
    ThreeOf3 value ->  formatPercentage value


viewport :: Element -> Maybe String
viewport element = case Svg.viewport element of
    Just (OneOf1 value) -> Just $ formatViewport value
    Nothing -> Nothing

