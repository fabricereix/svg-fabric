module Svg.Attribute.Rect where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.Rect as Rect


x :: Element -> String
x element = case Rect.x element of
    OneOf2 value ->  formatLength value
    TwoOf2 value ->  formatPercentage value


y :: Element -> String
y element = case Rect.y element of
    OneOf2 value ->  formatLength value
    TwoOf2 value ->  formatPercentage value


width :: Element -> String
width element = case Rect.width element of
    OneOf3 value ->  formatAuto value
    TwoOf3 value ->  formatLength value
    ThreeOf3 value ->  formatPercentage value


height :: Element -> String
height element = case Rect.height element of
    OneOf3 value ->  formatAuto value
    TwoOf3 value ->  formatLength value
    ThreeOf3 value ->  formatPercentage value


fill :: Element -> Maybe String
fill element = case Rect.fill element of
    Just (OneOf1 value) -> Just $ formatPaint value
    Nothing -> Nothing

stroke :: Element -> Maybe String
stroke element = case Rect.stroke element of
    Just (OneOf1 value) -> Just $ formatPaint value
    Nothing -> Nothing

