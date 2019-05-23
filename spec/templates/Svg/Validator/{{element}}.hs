{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.{{element.name|capitalize}} where

import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Map as Map
import           Svg.Validator.Core
import qualified Svg.Types.Parser as Parser
import           Svg.Types.Format
import           Text.XML
import           Prelude hiding (id)

validateAttributes :: Map.Map Name T.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, T.Text) -> [Error]
{% for attribute in element.attributes %}{% set name = attribute.name.replace('-','') + ("'" if attribute.name in ['class','type'] else '') %}
validateAttribute (Name { nameLocalName="{{attribute.name}}" }, value) = {{name}} value
{% endfor %}validateAttribute (name, _) = [InvalidAttribute "{{element.name}}" name]


{% for attribute in element.attributes %}{% set name = attribute.name.replace('-','') + ("'" if attribute.name in ['class','type'] else '') %}
{{name}} :: T.Text -> [Error]
{% if attribute.default != None %}{{name}} "{{attribute.default}}" = [AttributeDefault "{{element.name}}" "{{attribute.name}}"]
{% endif %}{{name}} v =
  {% for i,type in enumerate(attribute.type) %}case Parser.{{type}} (cs v) of
      {{' '*4*i}}Right parsed -> if format{{type|capitalize}} parsed == (cs v) then [] else [AttributeFormat "{{element.name}}" "{{attribute.name}}" v]
      {{' '*4*i}}Left _ -> {% endfor %}[InvalidAttributeValue "{{element.name}}" "{{attribute.name}}" v]

{% endfor %}



