module AttributeParser where

import Svg.Element

type Attribute = (String, String)
type Error = String

attribute :: Element -> Attribute -> Either Error Element
{% for element in elements %}
{% for attribute in element.attributes %}attribute elem@({{element.name|capitalize}}) ("{{attribute.name}}", value) {% for n in range(element|max_attribute_name-attributes|size-attribute.name|length) %} {% endfor %}= Right elem
{% endfor %}attribute elem@({{element.name|capitalize}}) (name, _)       {% for n in range(element|max_attribute_name-4) %} {% endfor %}= Left $ "Invalid attribute " ++ name ++ " for element {{element.name}}"
{% endfor %}