module Svg.Parser where

import Svg.Elements
{% for element in elements %}import qualified Svg.Parser.{{element.name | capitalize}} as {{element.name | capitalize}}
{% endfor %}


type Attribute = (String, String)


{% set max_element_length = max(map(len,map(name, elements))) %}

-- paramters:
--   element name
--   svg attributes
-- return error or parsed elements
-- (create another function if you can accept unused attributes)
parse :: String -> [Attribute] -> Either String Element
{% for element in elements %}parse "{{element.name}}"{{' ' * (max_element_length - len(element.name))}} attrs = {{element.name | capitalize}}.parse attrs
{% endfor %}parse name{{' '* (max_element_length - 2)}} _     = Left $ "Invalid element '" ++ name ++ "'"




attribute :: Element -> (String,String) -> Either String Element
attribute = undefined
