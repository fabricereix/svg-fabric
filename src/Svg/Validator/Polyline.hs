{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Polyline where

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
validateAttribute (Name { nameLocalName="points" }, value) = points value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
validateAttribute (name, _) = [InvalidAttribute "polyline" name]


points :: Text.Text -> [Error]
points v =
  case Parser.points (cs v) of
      Right parsed -> if formatPoints parsed == (cs v) then [] else [AttributeFormat "polyline" "points" v]
      Left _ -> [InvalidAttributeValue "polyline" "points" v]

fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "polyline" "fill" v]
      Left _ -> [InvalidAttributeValue "polyline" "fill" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "polyline" "stroke" v]
      Left _ -> [InvalidAttributeValue "polyline" "stroke" v]

strokewidth :: Text.Text -> [Error]
strokewidth "1" = [AttributeDefault "polyline" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "polyline" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "polyline" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "polyline" "stroke-width" v]




