{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.{{element.name|capitalize}} where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML


{% for attribute in element.attributes %}
{{attribute.name}} :: String -> Element -> Either String Element
{{attribute.name}} _ element@Element {
    elementName=Name { nameLocalName="{{element.name}}" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="{{attribute.name}}", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute {{attribute.name}} already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
{{attribute.name}} _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a {{element.name}} element"
{% endfor %}


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

