module Svg.Parser.{{element.name|capitalize}} where

import Svg.Elements
import Svg.Combinator.{{element.name | capitalize}}


parse :: [(String,String)] -> Either String Element
parse [] = Right default{{element.name|capitalize}}
parse (attr:attrs) = case parse attrs of
    Left e -> Left e
    Right element -> parseAttribute element attr


parseAttribute :: Element -> (String,String) -> Either String Element
{% for attribute in element.attributes %}parseAttribute element@({{element.name| capitalize}} _{% for _ in element.attributes %} _{% endfor %}) ("{{attribute.name}}", v) = {{attribute.name}} v element
{% endfor %}parseAttribute ({{element.name| capitalize}} _{% for _ in element.attributes %} _{% endfor %}) (name, _) = Left $ "attribute " ++ name ++ " is not defined for {{element.name}}"
parseAttribute _ _ = error "should have a {{element.name | capitalize}}!"

