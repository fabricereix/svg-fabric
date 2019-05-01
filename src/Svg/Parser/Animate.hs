module Svg.Parser.Animate where

import Svg.Elements
import Svg.Combinator.Animate


parse :: [(String,String)] -> Either String Element
parse [] = Right defaultAnimate
parse (attr:attrs) = case parse attrs of
    Left e -> Left e
    Right element -> parseAttribute element attr


parseAttribute :: Element -> (String,String) -> Either String Element
parseAttribute element@(Animate _ _) ("fill", v) = fill v element
parseAttribute (Animate _ _) (attributeName, _) = Left $ "attribute " ++ attributeName ++ " is not defined for animate"
parseAttribute _ _ = error "should have a Animate!"
