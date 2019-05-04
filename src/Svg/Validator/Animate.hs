{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Animate where

import Svg.Validator.Core
import Text.XML
-- import Data.XML.Types
import qualified Data.Text as Text
import qualified Data.Map as Map

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (name, _) = [InvalidAttribute "animate" name]


fill :: Text.Text -> [Error]
fill "remove" = [AttributeDefault "animate" "fill"]
fill _ = []


