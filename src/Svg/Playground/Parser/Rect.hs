module Svg.Playground.Parser.Rect where

import Svg.Playground.Elements
import Svg.Playground.Setter.Rect
--import Svg.Types.Core
import qualified Svg.Types.Parser as Parser

attribute :: Element -> (String, String) -> Either String Element
attribute element@(Rect _ _) ("x", value) =
    case Parser.length value of
        Right v1 -> Right $ x element (OneOf2 v1)
        Left _ -> case Parser.percentage value of
            Right v2 -> Right $ x element (TwoOf2 v2)
            Left _ -> Left $ "Invalid attribute value " ++ value ++ " for attribute x for element rect"
attribute element@(Rect _ _) ("fill", value) =
    case Parser.paint value of
        Right x1 -> Right $ fill element (Just (OneOf1 x1))
        Left _ -> Left $ "Invalid attribute value " ++ value ++ " for attribute fill for element rect"
attribute (Rect _ _) (name, _) = Left $ "Invalid attribute " ++ name ++ " for element rect"
attribute _                   _ = Left "should be call for rect element only"



-- return element and attributes not used
attributes :: Element -> [(String,String)] -> (Element,  [(String,String)])
attributes = undefined


