{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Rect where

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
validateAttribute (Name { nameLocalName="x" }, value) = x value
validateAttribute (Name { nameLocalName="y" }, value) = y value
validateAttribute (Name { nameLocalName="width" }, value) = width value
validateAttribute (Name { nameLocalName="height" }, value) = height value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
validateAttribute (name, _) = [InvalidAttribute "rect" name]


x :: Text.Text -> [Error]
x "0" = [AttributeDefault "rect" "x"]
x v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "x" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "x" v]
          Left _ -> [InvalidAttributeValue "rect" "x" v]

y :: Text.Text -> [Error]
y "0" = [AttributeDefault "rect" "y"]
y v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "y" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "y" v]
          Left _ -> [InvalidAttributeValue "rect" "y" v]

width :: Text.Text -> [Error]
width "auto" = [AttributeDefault "rect" "width"]
width v =
  case Parser.auto (cs v) of
      Right parsed -> if formatAuto parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
      Left _ -> case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
              Left _ -> [InvalidAttributeValue "rect" "width" v]

height :: Text.Text -> [Error]
height "auto" = [AttributeDefault "rect" "height"]
height v =
  case Parser.auto (cs v) of
      Right parsed -> if formatAuto parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
      Left _ -> case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
              Left _ -> [InvalidAttributeValue "rect" "height" v]

fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "rect" "fill" v]
      Left _ -> [InvalidAttributeValue "rect" "fill" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "rect" "stroke" v]
      Left _ -> [InvalidAttributeValue "rect" "stroke" v]

strokewidth :: Text.Text -> [Error]
strokewidth "1" = [AttributeDefault "rect" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "rect" "stroke-width" v]




