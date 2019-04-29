module Svg.Playground.Combinator.Rect where

import Svg.Playground.Elements
import qualified Svg.Playground.Setter.Rect as Rect
--import Svg.Types.Core
import qualified Svg.Types.Parser as Parser

x :: String -> Element -> Either String Element
x value element@(Rect _ _) =
    case Parser.length value of
        Right v1 -> Right $ Rect.x element (OneOf2 v1)
        Left _ -> case Parser.percentage value of
            Right v2 -> Right $ Rect.x element (TwoOf2 v2)
            Left _ -> Left $ "Invalid attribute value '" ++ value ++ "'' for attribute x for element rect"
x _ _ = Left $ "Should have an element rect"

fill :: String -> Element -> Either String Element
fill value element@(Rect _ _) =
    case Parser.paint value of
        Right x1 -> Right $ Rect.fill element (Just (OneOf1 x1))
        Left _ -> Left $ "Invalid attribute value '" ++ value ++ "' for attribute fill for element rect"
fill _ _ = Left $ "Should have an element rect"






