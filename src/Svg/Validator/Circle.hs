{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Circle where

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
validateAttribute (Name { nameLocalName="cx" }, value) = cx value
validateAttribute (Name { nameLocalName="cy" }, value) = cy value
validateAttribute (Name { nameLocalName="r" }, value) = r value
validateAttribute (Name { nameLocalName="pathLength" }, value) = pathLength value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (name, _) = [InvalidAttribute "circle" name]


cx :: Text.Text -> [Error]
cx "0" = [AttributeDefault "circle" "cx"]
cx v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "circle" "cx" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "circle" "cx" v]
          Left _ -> [InvalidAttributeValue "circle" "cx" v]

cy :: Text.Text -> [Error]
cy "0" = [AttributeDefault "circle" "cy"]
cy v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "circle" "cy" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "circle" "cy" v]
          Left _ -> [InvalidAttributeValue "circle" "cy" v]

r :: Text.Text -> [Error]
r "0" = [AttributeDefault "circle" "r"]
r v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "circle" "r" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "circle" "r" v]
          Left _ -> [InvalidAttributeValue "circle" "r" v]

pathLength :: Text.Text -> [Error]
pathLength v =
  case Parser.number (cs v) of
      Right parsed -> if formatNumber parsed == (cs v) then [] else [AttributeFormat "circle" "pathLength" v]
      Left _ -> [InvalidAttributeValue "circle" "pathLength" v]

fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "circle" "fill" v]
      Left _ -> [InvalidAttributeValue "circle" "fill" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "circle" "stroke" v]
      Left _ -> [InvalidAttributeValue "circle" "stroke" v]




