module Svg.AttributeParser where

import Svg.Elements
import Svg.Types.Core

type Attribute = (String, String)
type Error = String

attribute :: Element -> Attribute -> Either Error Element
{% for element in elements %}
{% for attribute in element.attributes %}attribute elem@({{element.name|capitalize}}{}) ("{{attribute.name}}", value) =
{% include 'attribute_value_parser.tmpl' %}
{% endfor %}attribute elem@({{element.name|capitalize}}{}) (name, _) = Left $ "Invalid attribute " ++ name ++ " for element {{element.name}}"
{% endfor %}
