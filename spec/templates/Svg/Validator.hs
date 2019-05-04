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
  } = {{element.name|capitalize}}.validateAttributes attributes
{% endfor %}
validate Element {elementName=elemName }  = [InvalidElement elemName]
