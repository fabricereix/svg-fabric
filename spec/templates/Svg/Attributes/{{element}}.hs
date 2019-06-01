{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.{{element.name|capitalize}} where

all :: [String]
all = [
{% for attribute in element.attributes %}  {%if loop.index == 1 %}  {% else %}, {% endif %}"{{attribute.name}}"
{% endfor %}  ]


