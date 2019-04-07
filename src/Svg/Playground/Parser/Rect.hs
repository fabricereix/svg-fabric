module Svg.Playground.Parser.Rect where

import Svg.Playground.Elements
import qualified Svg.Playground.Combinator.Rect as Rect
--import Svg.Types.Core


attribute :: (String,String) -> Element  -> Either String Element
attribute ("x", value) element@(Rect _ _)  = Rect.x value element
attribute ("fill", value) element@(Rect _ _)  = Rect.fill value element
attribute (name, _) (Rect _ _)  = Left $ "Invalid attribute " ++ name ++ " for element rect"
attribute _                   _ = Left "should be call for rect element only"



-- return element + list of errors
attributes :: [(String,String)] -> (Element, [String])
attributes attrs = foldl setAttr (defaultRect, []) attrs
    where setAttr (element, errors) attr = case attribute attr element of
                                               Right newElement -> (newElement, errors)
                                               Left e -> (element, errors ++ [e])

