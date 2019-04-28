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
{% for element in elements %}parse "{{element.name}}" _ = undefined
{% endfor %}parse name _ = Left $ "Invalid element '" ++ name ++ "'"


