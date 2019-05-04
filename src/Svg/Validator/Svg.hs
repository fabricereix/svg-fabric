{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Svg where

import           Data.String.Conversions
import qualified Data.Text as Text
import qualified Data.Map as Map
import           Svg.Validator.Core
import qualified Svg.Types.Parser as Parser
import           Svg.Types.Format
import           Text.XML

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
validateAttribute (Name { nameLocalName="width" }, value) = width value
validateAttribute (Name { nameLocalName="height" }, value) = height value
validateAttribute (Name { nameLocalName="viewport" }, value) = viewport value
validateAttribute (name, _) = [InvalidAttribute "svg" name]


width :: Text.Text -> [Error]
width v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "svg" "width" v]
      Left _ -> [InvalidAttributeValue "svg" "width" v]

height :: Text.Text -> [Error]
height v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "svg" "height" v]
      Left _ -> [InvalidAttributeValue "svg" "height" v]

viewport :: Text.Text -> [Error]
viewport v =
  case Parser.viewport (cs v) of
      Right parsed -> if formatViewport parsed == (cs v) then [] else [AttributeFormat "svg" "viewport" v]
      Left _ -> [InvalidAttributeValue "svg" "viewport" v]




