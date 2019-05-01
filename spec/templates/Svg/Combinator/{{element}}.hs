module Svg.Combinator.{{element.name|capitalize}} where
import Svg.Elements
import qualified Svg.Setter.{{element.name|capitalize}} as {{element.name | capitalize}}
import qualified Svg.Types.Parser as Parser


{% for attribute in element.attributes %}
{{attribute.name}} :: String -> Element -> Either String Element
{{attribute.name}} v element@({{element.name| capitalize}} _{% for _ in element.attributes %} _{% endfor %}) =
  {% for i,type in enumerate(attribute.type) %}case Parser.{{type}} v of
      {{' '*4*i}}Right parsed -> Right $ {{element.name| capitalize}}.{{attribute.name}} element {% if attribute.default == None %}$ Just {% endif %}$ {{wordDigit(i+1)}}Of{{attribute.type|length}} parsed
      {{' '*4*i}}Left _ -> {% endfor %}Left $ "Can not parse value \"" ++ v ++ "\" for attribute {{attribute.name}}"
{{attribute.name}} _ _ = error "should have a {{element.name | capitalize}} element!"
{% endfor %}

