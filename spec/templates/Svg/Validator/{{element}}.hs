{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.{{element.name|capitalize}} where

import           Data.String.Conversions
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Svg.Validator.Core
import qualified Svg.Types.Parser as Parser
import           Svg.Types.Format
import           Text.XML
import           Prelude hiding (id)

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
{% for attribute in element.attributes %}validateAttribute (Name { nameLocalName="{{attribute.name}}" }, value) = {{attribute.name.replace('-','')}} value
{% endfor %}validateAttribute (name, _) = [InvalidAttribute "{{element.name}}" name]


{% for attribute in element.attributes %}{{attribute.name.replace('-','')}} :: Text.Text -> [Error]
{% if attribute.default != None %}{{attribute.name.replace('-','')}} "{{attribute.default}}" = [AttributeDefault "{{element.name}}" "{{attribute.name}}"]
{% endif %}{{attribute.name.replace('-','')}} v =
  {% for i,type in enumerate(attribute.type) %}case Parser.{{type}} (cs v) of
      {{' '*4*i}}Right parsed -> if format{{type|capitalize}} parsed == (cs v) then [] else [AttributeFormat "{{element.name}}" "{{attribute.name}}" v]
      {{' '*4*i}}Left _ -> {% endfor %}[InvalidAttributeValue "{{element.name}}" "{{attribute.name}}" v]

{% endfor %}



