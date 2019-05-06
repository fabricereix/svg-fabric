{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.{{element.name|capitalize}} where
import Text.XML
import Data.String.Conversions
import qualified Svg.DefaultElements as Default

{% for attribute in element.attributes %}
{{attribute.name}} :: String -> Element -> Either String Element
{{attribute.name}} _ Element {
    elementName=Name { nameLocalName="{{element.name}}" }
  } = Right Default.{{element.name}}
{{attribute.name}} _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a {{element.name}} element"
{% endfor %}



