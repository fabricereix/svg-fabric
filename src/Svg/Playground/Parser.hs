module Svg.Playground.Parser where

import Svg.Playground.Elements
import qualified Svg.Playground.Parser.Rect as Rect




parse :: String -> [(String, String)] -> Either String (Element,[String])
parse "rect" attrs = Right $ Rect.attributes attrs
parse name _ = Left $ "Invalid element '" ++ name ++ "'"



