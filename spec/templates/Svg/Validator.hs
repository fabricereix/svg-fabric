{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator where

import Text.XML
--import Data.XML.Types
import Svg.Validator.Core

{% for element in elements %}import qualified Svg.Validator.{{element.name| capitalize}} as {{element.name | capitalize}}
{% endfor %}


validate :: Element -> [Error]
{% for element in elements %}validate Element {
    elementName=Name {
        nameLocalName="{{element.name}}"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = {{element.name|capitalize}}.validateAttributes attributes ++ validateChildren children

{% endfor %}
validate Element {elementName=elemName }  = [InvalidElement elemName]


validateChildren :: [Node] -> [Error]
validateChildren [] = []
validateChildren (x:xs) = case x of
    NodeElement element -> validate element
    _  -> []
  ++ validateChildren xs




