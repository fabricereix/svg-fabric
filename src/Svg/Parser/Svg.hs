module Svg.Parser.Svg where

import Svg.Elements
import Svg.Combinator.Svg


parse :: [(String,String)] -> Either String Element
parse [] = Right defaultSvg
parse (attr:attrs) = case parse attrs of
    Left e -> Left e
    Right element -> parseAttribute element attr


parseAttribute :: Element -> (String,String) -> Either String Element
parseAttribute element@(Svg _ _ _ _) ("width", v) = width v element
parseAttribute element@(Svg _ _ _ _) ("height", v) = height v element
parseAttribute element@(Svg _ _ _ _) ("viewport", v) = viewport v element
parseAttribute (Svg _ _ _ _) (name, _) = Left $ "attribute " ++ name ++ " is not defined for svg"
parseAttribute _ _ = error "should have a Svg!"
