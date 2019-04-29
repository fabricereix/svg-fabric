module Svg.Getter.{{element.name|capitalize}} where
import Svg.Elements
import Svg.Types.Core

{% for attribute in element.attributes %}
{{attribute.name}} :: Element -> {% if attribute.default == None %}Maybe ({% endif %}OneOf{{attribute.type|length}}{% for t in attribute.type %} {{capitalize(t)}}{% endfor %}{% if attribute.default == None %}){% endif %}
{{attribute.name}} ({{element.name| capitalize}} _{% for i,attribute2 in enumerate(element.attributes) %} {%if attribute2.name == attribute.name %}v{% else %}_{% endif %}{% endfor %}) = v
{{attribute.name}} _  = error "Element should be {{element.name|capitalize}}!"
{% endfor %}

