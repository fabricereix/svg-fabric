module Svg.Elements where

data Element =
    {% for element in elements %}{% if loop.index > 1 %}    | {% endif %}{{element.name|capitalize}} {
        {% for attribute in element.attributes %}{% if loop.index > 1 %}  , {% endif %}{{attribute.name}} :: OneOf{{attribute.type|length}}{% for t in attribute.type %} {{t|capitalize}}{% endfor %}
    {% endfor %}}
{% endfor %}
ELEMENT_GROUPS = {
    'graphics': ['rect', 'circle'],
    'all': [elem for elem in ELEMENTS]
}