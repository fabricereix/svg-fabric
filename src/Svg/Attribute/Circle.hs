module Svg.Attribute.Circle where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.Circle as Circle


cx :: Element -> String
cx element = case Circle.cx element of
    OneOf2 value ->  formatLength value
    TwoOf2 value ->  formatPercentage value


cy :: Element -> String
cy element = case Circle.cy element of
    OneOf2 value ->  formatLength value
    TwoOf2 value ->  formatPercentage value


r :: Element -> String
r element = case Circle.r element of
    OneOf2 value ->  formatLength value
    TwoOf2 value ->  formatPercentage value


pathLength :: Element -> Maybe String
pathLength element = case Circle.pathLength element of
    Just (OneOf1 value) -> Just $ formatNumber value
    Nothing -> Nothing

fill :: Element -> Maybe String
fill element = case Circle.fill element of
    Just (OneOf1 value) -> Just $ formatPaint value
    Nothing -> Nothing

stroke :: Element -> Maybe String
stroke element = case Circle.stroke element of
    Just (OneOf1 value) -> Just $ formatPaint value
    Nothing -> Nothing

