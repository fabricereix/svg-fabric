{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Animate where

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
validateAttribute (name, _) = [InvalidAttribute "animate" name]


fill :: Text.Text -> [Error]
fill "remove" = [AttributeDefault "animate" "fill"]
fill v =
  case Parser.removeFreeze (cs v) of
      Right parsed -> if formatRemovefreeze parsed == (cs v) then [] else [AttributeFormat "animate" "fill" v]
      Left _ -> [InvalidAttributeValue "animate" "fill" v]




