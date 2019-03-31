module Svg.Elements where

data Element =
    {% for element in elements %}{% if loop.index > 1 %}    | {% endif %}{{element.name|capitalize}} {
        {% for attribute in element.attributes %}{% if loop.index > 1 %}  , {% endif %}_{{attribute.name}} :: OneOf{{attribute.type|length}}{% for t in attribute.type %} {{t|camel_case}}{% endfor %}
    {% endfor %}}
{% endfor %}