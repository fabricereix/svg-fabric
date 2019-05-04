{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Svg where

import Svg.Validator.Core
import Text.XML
-- import Data.XML.Types
import qualified Data.Text as Text
import qualified Data.Map as Map

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
validateAttribute (Name { nameLocalName="width" }, value) = width value
validateAttribute (Name { nameLocalName="height" }, value) = height value
validateAttribute (Name { nameLocalName="viewport" }, value) = viewport value
validateAttribute (name, _) = [InvalidAttribute "svg" name]


width :: Text.Text -> [Error]
width "None" = [AttributeDefault "svg" "width"]
width _ = []

height :: Text.Text -> [Error]
height "None" = [AttributeDefault "svg" "height"]
height _ = []

viewport :: Text.Text -> [Error]
viewport "None" = [AttributeDefault "svg" "viewport"]
viewport _ = []


