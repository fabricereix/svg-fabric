{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.{{element.name|capitalize}} where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format


{% for attribute in element.attributes %}
{{attribute.name}} :: Text.Text -> Element -> Either String Element
{{attribute.name}} v element@Element {
    elementName=Name { nameLocalName="{{element.name}}" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="{{attribute.name}}", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute {{attribute.name}} already set"
      else {% for i,type in enumerate(attribute.type) %}case Parser.{{type}} (cs v) of
          {{' '*4*i}}Right parsed -> if format{{type|capitalize}} parsed == (cs v)
          {{' '*4*i}}                then Right (addAttribute element ("{{attribute.name}}",v))
          {{' '*4*i}}                else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (format{{type|capitalize}} parsed)))
          {{' '*4*i}}Left _ -> {% endfor %}Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute {{attribute.name}}")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("{{attribute.name}}", "1")]
--      }
{{attribute.name}} _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a {{element.name}} element"
{% endfor %}


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
