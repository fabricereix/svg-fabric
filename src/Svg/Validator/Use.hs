{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Use where

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
validateAttribute (Name { nameLocalName="href" }, value) = href value
validateAttribute (Name { nameLocalName="x" }, value) = x value
validateAttribute (Name { nameLocalName="y" }, value) = y value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="stroke" }, value) = stroke value
validateAttribute (name, _) = [InvalidAttribute "use" name]


href :: Text.Text -> [Error]
href v =
  case Parser.id (cs v) of
      Right parsed -> if formatId parsed == (cs v) then [] else [AttributeFormat "use" "href" v]
      Left _ -> [InvalidAttributeValue "use" "href" v]

x :: Text.Text -> [Error]
x v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "use" "x" v]
      Left _ -> [InvalidAttributeValue "use" "x" v]

y :: Text.Text -> [Error]
y v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "use" "y" v]
      Left _ -> [InvalidAttributeValue "use" "y" v]

fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "use" "fill" v]
      Left _ -> [InvalidAttributeValue "use" "fill" v]

stroke :: Text.Text -> [Error]
stroke v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "use" "stroke" v]
      Left _ -> [InvalidAttributeValue "use" "stroke" v]




