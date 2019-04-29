module Svg.Setter.{{element.name|capitalize}} where
import Svg.Elements
import Svg.Types.Core

{% for attribute in element.attributes %}
{{attribute.name}} :: Element -> {% if attribute.default == None %}Maybe ({% endif %}OneOf{{attribute.type|length}}{% for t in attribute.type %} {{capitalize(t)}}{% endfor %}{% if attribute.default == None %}){% endif %} -> Element
{{attribute.name}} ({{element.name| capitalize}} a0{% for i,attribute2 in enumerate(element.attributes) %} {%if attribute2.name == attribute.name %}_{% else %}a{{i+1}}{% endif %}{% endfor %}) v  = ({{element.name| capitalize}} a0{% for i,attribute2 in enumerate(element.attributes) %} {% if attribute2.name == attribute.name %}v{% else %}a{{i+1}}{% endif %}{% endfor %})
{{attribute.name}} _ _  = error "Element should be {{element.name|capitalize}}!"
{% endfor %}

