module Svg.Parser where

import Svg.Elements
-- import qualified Svg.Playground.Parser.Rect as Rect


type Attribute = (String, String)


-- paramters:
--   element name
--   svg attributes
-- return error or parsed elements
-- (create another function if you can accept unused attributes)
parse :: String -> [Attribute] -> Either String Element
parse "animate" _ = undefined
parse "circle" _ = undefined
parse "rect" _ = undefined
parse "svg" _ = undefined
parse name _ = Left $ "Invalid element '" ++ name ++ "'"

