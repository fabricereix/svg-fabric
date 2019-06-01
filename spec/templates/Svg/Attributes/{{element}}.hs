{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.{{element.name|capitalize}} where

all :: [String]
all = [
{% for attribute in element.attributes %}  {%if loop.index == 1 %}  {% else %}, {% endif %}"{{attribute.name}}"
{% endfor %}  ]


defaultValue :: String -> Maybe String
{% for attribute in element.attributes %}defaultValue "{{attribute.name}}" = {% if attribute.default %} Nothing{% else %}Just "{{attribute.default}}"{% endif %}
{% endfor %}defaultValue name = error $ "invalid attribute " ++ name ++ " for element {{element.name}}"



