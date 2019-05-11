{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Symbol where

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
validateAttribute (Name { nameLocalName="id" }, value) = id value
validateAttribute (name, _) = [InvalidAttribute "symbol" name]


width :: Text.Text -> [Error]
width v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "symbol" "width" v]
      Left _ -> [InvalidAttributeValue "symbol" "width" v]

height :: Text.Text -> [Error]
height v =
  case Parser.length (cs v) of
      Right parsed -> if formatLength parsed == (cs v) then [] else [AttributeFormat "symbol" "height" v]
      Left _ -> [InvalidAttributeValue "symbol" "height" v]

viewBox :: Text.Text -> [Error]
viewBox v =
  case Parser.viewbox (cs v) of
      Right parsed -> if formatViewbox parsed == (cs v) then [] else [AttributeFormat "symbol" "viewBox" v]
      Left _ -> [InvalidAttributeValue "symbol" "viewBox" v]

id :: Text.Text -> [Error]
id v =
  case Parser.id (cs v) of
      Right parsed -> if formatId parsed == (cs v) then [] else [AttributeFormat "symbol" "id" v]
      Left _ -> [InvalidAttributeValue "symbol" "id" v]




