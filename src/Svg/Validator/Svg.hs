{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Svg where

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
validateAttribute (Name { nameLocalName="width" }, value) = width value
validateAttribute (Name { nameLocalName="height" }, value) = height value
validateAttribute (Name { nameLocalName="viewBox" }, value) = viewBox value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (Name { nameLocalName="stroke-width" }, value) = strokewidth value
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

viewBox :: Text.Text -> [Error]
viewBox v =
  case Parser.viewbox (cs v) of
      Right parsed -> if formatViewbox parsed == (cs v) then [] else [AttributeFormat "svg" "viewBox" v]
      Left _ -> [InvalidAttributeValue "svg" "viewBox" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "svg" "stroke" v]
      Left _ -> [InvalidAttributeValue "svg" "stroke" v]

strokewidth :: Text.Text -> [Error]
strokewidth "1" = [AttributeDefault "svg" "stroke-width"]
strokewidth v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "svg" "stroke-width" v]
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> if formatPercentage parsed == (cs v) then [] else [AttributeFormat "svg" "stroke-width" v]
          Left _ -> [InvalidAttributeValue "svg" "stroke-width" v]




