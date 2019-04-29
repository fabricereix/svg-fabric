module Svg.Attribute.{{element.name|capitalize}} where
import Svg.Elements
import Svg.Types.Format
import qualified Svg.Getter.{{element.name | capitalize}} as {{element.name | capitalize}}

{% for attribute in element.attributes %}
{{attribute.name}} :: Element -> {% if attribute.default == None %}Maybe {% endif %}String
{{attribute.name}} element = case {{element.name| capitalize}}.{{attribute.name}} element of
{% for i,type in enumerate(attribute.type) %}    {% if attribute.default == None %}Just ({% endif %}{{wordDigit(i+1)}}Of{{attribute.type| length}} value{%if attribute.default == None %}){% endif %} -> {% if attribute.default == None %}Just ${% endif %} format{{capitalize(type)}} value
{% if attribute.default ==None %}    Nothing -> Nothing{% endif %}{% endfor %}
{% endfor %}

