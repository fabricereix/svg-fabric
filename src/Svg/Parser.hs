module Svg.Parser where

import Svg.Elements
import qualified Svg.Parser.Animate as Animate
import qualified Svg.Parser.Circle as Circle
import qualified Svg.Parser.Rect as Rect
import qualified Svg.Parser.Svg as Svg



type Attribute = (String, String)




-- paramters:
--   element name
--   svg attributes
-- return error or parsed elements
-- (create another function if you can accept unused attributes)
parse :: String -> [Attribute] -> Either String Element
parse "animate" attrs = Animate.parse attrs
parse "circle"  attrs = Circle.parse attrs
parse "rect"    attrs = Rect.parse attrs
parse "svg"     attrs = Svg.parse attrs
parse elementName      _     = Left $ "Invalid element '" ++ elementName ++ "'"




attribute :: Element -> (String,String) -> Either String Element
attribute = undefined