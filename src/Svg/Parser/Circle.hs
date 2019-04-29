module Svg.Parser.Circle where

import Svg.Elements
import Svg.Combinator.Circle


parse :: [(String,String)] -> Either String Element
parse [] = Right defaultCircle
parse (attr:attrs) = case parse attrs of
    Left e -> Left e
    Right element -> parseAttribute element attr


parseAttribute :: Element -> (String,String) -> Either String Element
parseAttribute element@(Circle _ _ _ _ _ _) ("cx", v) = cx v element
parseAttribute element@(Circle _ _ _ _ _ _) ("cy", v) = cy v element
parseAttribute element@(Circle _ _ _ _ _ _) ("r", v) = r v element
parseAttribute element@(Circle _ _ _ _ _ _) ("pathLength", v) = pathLength v element
parseAttribute element@(Circle _ _ _ _ _ _) ("fill", v) = fill v element
parseAttribute (Circle _ _ _ _ _ _) (name, _) = Left $ "attribute " ++ name ++ " is not defined for circle"
parseAttribute _ _ = error "should have a Circle!"
