module Svg.Attribute where

import Svg.Elements
import Data.Maybe

{% for element in elements %}import qualified Svg.Attribute.{{element.name | capitalize}} as {{element.name | capitalize}}
{% endfor %}

attributes :: Element -> [(String,String)]
{% for element in elements %}attributes element@({{element.name|capitalize}}{{ ' _' * (len(element.attributes) + 1)}}) =
{% for i,attribute in enumerate(element.attributes) %}  {% if i==0 %}  {% else %}++{% endif %} {% if attribute.default == None %}map (\v->("{{attribute.name}}",v)) (maybeToList $ {{element.name|capitalize}}.{{attribute.name}} element){%else%}case {{element.name | capitalize}}.{{attribute.name}} element of "{{attribute.default}}" -> []; value -> [("{{attribute.name}}", value)]{%endif%}
{% endfor %}
{% endfor %}

