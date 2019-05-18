{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.G where

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
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (Name { nameLocalName="transform" }, value) = transform value
validateAttribute (name, _) = [InvalidAttribute "g" name]


fill :: Text.Text -> [Error]
fill v =
  case Parser.paint (cs v) of
      Right parsed -> if formatPaint parsed == (cs v) then [] else [AttributeFormat "g" "fill" v]
      Left _ -> [InvalidAttributeValue "g" "fill" v]

transform :: Text.Text -> [Error]
transform v =
  case Parser.transform (cs v) of
      Right parsed -> if formatTransform parsed == (cs v) then [] else [AttributeFormat "g" "transform" v]
      Left _ -> [InvalidAttributeValue "g" "transform" v]




