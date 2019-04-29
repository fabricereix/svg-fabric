module Svg.Parser.Rect where

import Svg.Elements
import Svg.Combinator.Rect


parse :: [(String,String)] -> Either String Element
parse [] = Right defaultRect
parse (attr:attrs) = case parse attrs of
    Left e -> Left e
    Right element -> parseAttribute element attr


parseAttribute :: Element -> (String,String) -> Either String Element
parseAttribute element@(Rect _ _ _ _ _ _) ("x", v) = x v element
parseAttribute element@(Rect _ _ _ _ _ _) ("y", v) = y v element
parseAttribute element@(Rect _ _ _ _ _ _) ("width", v) = width v element
parseAttribute element@(Rect _ _ _ _ _ _) ("height", v) = height v element
parseAttribute element@(Rect _ _ _ _ _ _) ("fill", v) = fill v element
parseAttribute (Rect _ _ _ _ _ _) (name, _) = Left $ "attribute " ++ name ++ " is not defined for rect"
parseAttribute _ _ = error "should have a Rect!"
