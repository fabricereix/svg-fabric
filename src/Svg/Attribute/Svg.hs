module Svg.Attribute.Svg where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.Svg as Svg


width :: Element -> Maybe String
width element = case Svg.width element of
    Just (OneOf1 value) -> Just $ formatLength value
    Nothing -> Nothing

height :: Element -> Maybe String
height element = case Svg.height element of
    Just (OneOf1 value) -> Just $ formatLength value
    Nothing -> Nothing

viewport :: Element -> Maybe String
viewport element = case Svg.viewport element of
    Just (OneOf1 value) -> Just $ formatViewport value
    Nothing -> Nothing

