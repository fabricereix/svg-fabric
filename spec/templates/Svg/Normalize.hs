{-# LANGUAGE OverloadedStrings     #-}
module Svg.Normalize where

{% for element in elements %}import qualified Svg.Normalize.{{element.name| capitalize}} as {{element.name| capitalize}}
{% endfor %}

normalize :: String -> String -> String -> Either String String
{% for element in elements %}normalize "{{element.name}}" name value = {{element.name| capitalize}}.normalize name value
{% endfor %}normalize name _ _ = error $ "element " ++ name ++ " not found"



