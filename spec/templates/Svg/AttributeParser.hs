module AttributeParser where

import Svg.Element

type Attribute = (String, String)
type Error = String

attribute :: Element -> Attribute -> Either Error Element
{% for element in elements %}
{% for attribute in element.attributes %}atttribute elem@({{element.name|capitalize}}) ("{{attribute.name}}", value) {% for n in range(10-attribute.name|length) %} {% endfor %}= Right elem
{% endfor %}atttribute elem@({{element.name|capitalize}}) (name, _)             = Left $ "Invalid attribute " ++ name ++ " for element " ++ {{element.name|capitalize}}
{% endfor %}