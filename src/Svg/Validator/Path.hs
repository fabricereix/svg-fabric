{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Path where

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
validateAttribute (Name { nameLocalName="d" }, value) = d value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="id" }, value) = id value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
validateAttribute (Name { nameLocalName="transform" }, value) = transform value
validateAttribute (name, _) = [InvalidAttribute "path" name]


d :: Text.Text -> [Error]
d v =
  case Parser.path (cs v) of
      Right parsed -> if formatPath parsed == (cs v) then [] else [AttributeFormat "path" "d" v]
      Left _ -> [InvalidAttributeValue "path" "d" v]

fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "path" "fill" v]
      Left _ -> [InvalidAttributeValue "path" "fill" v]

id :: Text.Text -> [Error]
id v =
  case Parser.id (cs v) of
      Right parsed -> if formatId parsed == (cs v) then [] else [AttributeFormat "path" "id" v]
      Left _ -> [InvalidAttributeValue "path" "id" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "path" "stroke" v]
      Left _ -> [InvalidAttributeValue "path" "stroke" v]

strokewidth :: Text.Text -> [Error]
strokewidth "1" = [AttributeDefault "path" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "path" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "path" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "path" "stroke-width" v]

transform :: Text.Text -> [Error]
transform v =
  case Parser.transform (cs v) of
      Right parsed -> if formatTransform parsed == (cs v) then [] else [AttributeFormat "path" "transform" v]
      Left _ -> [InvalidAttributeValue "path" "transform" v]




