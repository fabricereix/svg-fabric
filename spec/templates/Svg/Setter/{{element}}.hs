{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.{{element.name|capitalize}} where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML

{% for attribute in element.attributes %}{% for type in attribute.type %}{% for constructor in type_arguments(type) %}{% set setter = attribute.name.replace('-','') + ("" if len(attribute.type) == 1 and len(type_arguments(attribute.type[0])) == 1 else capitalize(constructor.split(' ')[0])) %}{% set arguments = constructor.split(' ')[1:]%}
{{setter}} :: {%for arg in arguments%} {{arg}} ->{% endfor%} Element -> Either String Element
{{setter}}{%for i in range(len(arguments))%} a{{i}}{%endfor%} element@Element {
    elementName=Name { nameLocalName="{{element.name}}" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="{{attribute.name}}", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute {{attribute.name}} already set"
      else Right $ addAttribute element ("{{attribute.name}}",cs $ format{{type| capitalize}} ({{constructor.split(' ')[0]}}{%for i in range(len(arguments))%} a{{i}}{% endfor%}))
{{setter}}{%for i in range(len(arguments))%} _{%endfor%} Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a {{element.name}} instead of " ++ cs name
{%endfor%}{% endfor%}{%endfor%}

hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }

