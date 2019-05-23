{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Text where

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

validateAttribute (Name { nameLocalName="dx" }, value) = dx value

validateAttribute (Name { nameLocalName="dy" }, value) = dy value

validateAttribute (Name { nameLocalName="class" }, value) = class' value

validateAttribute (Name { nameLocalName="fill" }, value) = fill value

validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value

validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
validateAttribute (name, _) = [InvalidAttribute "text" name]



x :: T.Text -> [Error]
x "0" = [AttributeDefault "text" "x"]
x v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "text" "x" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "text" "x" v]
          Left _ -> [InvalidAttributeValue "text" "x" v]


y :: T.Text -> [Error]
y "0" = [AttributeDefault "text" "y"]
y v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "text" "y" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "text" "y" v]
          Left _ -> [InvalidAttributeValue "text" "y" v]


dx :: T.Text -> [Error]
dx v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "text" "dx" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "text" "dx" v]
          Left _ -> [InvalidAttributeValue "text" "dx" v]


dy :: T.Text -> [Error]
dy v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "text" "dy" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "text" "dy" v]
          Left _ -> [InvalidAttributeValue "text" "dy" v]


class' :: T.Text -> [Error]
class' v =
  case Parser.classes (cs v) of
      Right parsed -> if formatClasses parsed == (cs v) then [] else [AttributeFormat "text" "class" v]
      Left _ -> [InvalidAttributeValue "text" "class" v]


fill :: T.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "text" "fill" v]
      Left _ -> [InvalidAttributeValue "text" "fill" v]


stroke :: T.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "text" "stroke" v]
      Left _ -> [InvalidAttributeValue "text" "stroke" v]


strokewidth :: T.Text -> [Error]
strokewidth "1" = [AttributeDefault "text" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "text" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "text" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "text" "stroke-width" v]




