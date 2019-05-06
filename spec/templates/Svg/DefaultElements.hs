{-# LANGUAGE OverloadedStrings     #-}
module Svg.DefaultElements where

import qualified Data.Map as Map
import           Text.XML

{% for element in elements %}{{element.name}} :: Element
{{element.name}} = Element {
      elementName=Name { nameLocalName="{{element.name}}", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

{% endfor %}
