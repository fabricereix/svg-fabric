{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Style where

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

validateAttribute (Name { nameLocalName="type'" }, value) = type' value
validateAttribute (name, _) = [InvalidAttribute "style" name]



type' :: T.Text -> [Error]
type' v =
  case Parser.contenttype (cs v) of
      Right parsed -> if formatContenttype parsed == (cs v) then [] else [AttributeFormat "style" "type'" v]
      Left _ -> [InvalidAttributeValue "style" "type'" v]




