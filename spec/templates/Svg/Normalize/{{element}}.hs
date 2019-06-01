{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.{{element.name|capitalize}} where

--import Data.XML.Types (hasAttribute)
--import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
--import qualified Data.Text as Text
--import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format


normalize :: String -> String -> Either String String
{% for attribute in element.attributes %}normalize "{{attribute.name}}" v =
  {% for i,type in enumerate(attribute.type) %}case Parser.{{type}} (cs v) of
      {{' '*4*i}}Right parsed -> Right $ format{{type | capitalize}} parsed
      {{' '*4*i}}Left _ -> {% endfor %} Left $ "Parsing error for attribute {{attribute.name}}"
{% endfor %}normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"

