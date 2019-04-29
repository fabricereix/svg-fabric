module Svg.Elements where

import Svg.Types.Core


data OneOf1 a = OneOf1 a
                deriving (Eq,Show)

data OneOf2 a b = OneOf2 a | TwoOf2 b
                  deriving (Eq,Show)

data OneOf3 a b c = OneOf3 a | TwoOf3 b | ThreeOf3 c
                  deriving (Eq,Show)


data Element =
    {% for element in elements %}{% if loop.index > 1 %}  | {% endif %}{{element.name|capitalize}}
      [Element]  -- children
{% for attribute in element.attributes %}      ({% if attribute.default == None %}Maybe ({% endif %}OneOf{{attribute.type|length}}{% for t in attribute.type %} {{capitalize(t)}}{% if attribute.default == None %}){% endif %}{% endfor %}) -- {{attribute.name}}
{% endfor %}
{% endfor %}  deriving (Show, Eq)


{% for element in elements %}default{{element.name | capitalize}} :: Element
default{{element.name | capitalize}} = {{element.name|capitalize}}
  [] -- children
{% for attribute in element.attributes %}  {% if attribute.default == None %}Nothing {% else %}(OneOf{{attribute.type|length}} {{attribute.default|default_attr}}){% endif %} -- {{attribute.name}}
{% endfor %}
{% endfor %}
