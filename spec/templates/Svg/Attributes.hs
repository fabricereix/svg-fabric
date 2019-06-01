{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes where

{% for element in elements %}import qualified Svg.Attributes.{{element.name| capitalize}} as {{element.name| capitalize}}
{% endfor %}

all :: String -> [String]
{% for element in elements %}all "{{element.name}}" =  {{element.name| capitalize}}.all
{% endfor %}all name = error $ "element " ++ name ++ " not found"


