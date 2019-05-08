module Svg.Setter.{{element.name|capitalize}} where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import Text.XML

{% for attribute in element.attributes %}{% for type in attribute.type %}{% for constructor in type_arguments(type) %}{% set setter = attribute.name + ("" if len(attribute.type) == 1 and len(type_arguments(attribute.type[0])) == 1 else capitalize(constructor.split(' ')[0])) %}
{{setter}} :: Element ->{%for arg in constructor.split(' ')[1:]%} {{arg}} ->{% endfor%} Element
{{setter}} = undefined
--{{setter}} element@Element {
--    elementName=Name { nameLocalName="{{element.name}}" }
--  } = undefined
--Element = undefined
{%endfor%}{% endfor%}{%endfor%}


