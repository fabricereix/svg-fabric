{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Rect where

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

validateAttribute (Name { nameLocalName="x" }, value) = x value

validateAttribute (Name { nameLocalName="y" }, value) = y value

validateAttribute (Name { nameLocalName="width" }, value) = width value

validateAttribute (Name { nameLocalName="height" }, value) = height value

validateAttribute (Name { nameLocalName="fill" }, value) = fill value

validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value

validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
validateAttribute (name, _) = [InvalidAttribute "rect" name]



x :: T.Text -> [Error]
x "0" = [AttributeDefault "rect" "x"]
x v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "x" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "x" v]
          Left _ -> [InvalidAttributeValue "rect" "x" v]


y :: T.Text -> [Error]
y "0" = [AttributeDefault "rect" "y"]
y v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "y" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "y" v]
          Left _ -> [InvalidAttributeValue "rect" "y" v]


width :: T.Text -> [Error]
width "auto" = [AttributeDefault "rect" "width"]
width v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
      Left _ -> case Parser.pixel (cs v) of
          Right parsed -> if formatPixel parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
              Left _ -> case Parser.auto (cs v) of
                  Right parsed -> if formatAuto parsed == (cs v) then [] else [AttributeFormat "rect" "width" v]
                  Left _ -> [InvalidAttributeValue "rect" "width" v]


height :: T.Text -> [Error]
height "auto" = [AttributeDefault "rect" "height"]
height v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
          Left _ -> case Parser.auto (cs v) of
              Right parsed -> if formatAuto parsed == (cs v) then [] else [AttributeFormat "rect" "height" v]
              Left _ -> [InvalidAttributeValue "rect" "height" v]


fill :: T.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "rect" "fill" v]
      Left _ -> [InvalidAttributeValue "rect" "fill" v]


stroke :: T.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "rect" "stroke" v]
      Left _ -> [InvalidAttributeValue "rect" "stroke" v]


strokewidth :: T.Text -> [Error]
strokewidth "1" = [AttributeDefault "rect" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "rect" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "rect" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "rect" "stroke-width" v]




