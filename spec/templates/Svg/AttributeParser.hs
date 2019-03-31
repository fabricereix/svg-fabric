module AttributeParser where

import Svg.Element

type Attribute = (String, String)
type Error = String

attribute :: Element -> Attribute -> Either Error Element
{% for element in elements %}
{% for attribute in element.attributes %}atttribute elem@({{element.name|capitalize}}) ("{{attribute.name}}", value) = Right elem
{% endfor %}atttribute elem@({{element.name|capitalize}}) {% for n in range(7-element.name|length) %} {% endfor %}(name, _) = Left $ "Invalid attribute " ++ name ++ " for element " ++ {{element.name|capitalize}}
{% endfor %}