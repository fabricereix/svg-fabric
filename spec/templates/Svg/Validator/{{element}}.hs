{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.{{element.name|capitalize}} where

import Svg.Validator.Core
import Text.XML
-- import Data.XML.Types
import qualified Data.Text as Text
import qualified Data.Map as Map

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
{% for attribute in element.attributes %}validateAttribute (Name { nameLocalName="{{attribute.name}}" }, value) = {{attribute.name}} value
{% endfor %}validateAttribute (name, _) = [InvalidAttribute "{{element.name}}" name]


{% for attribute in element.attributes %}{{attribute.name}} :: Text.Text -> [Error]
{{attribute.name}} "{{attribute.default}}" = [AttributeDefault "{{element.name}}" "{{attribute.name}}"]
{{attribute.name}} _ = []

{% endfor %}

