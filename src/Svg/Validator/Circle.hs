{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Circle where

import Svg.Validator.Core
import Text.XML
-- import Data.XML.Types
import qualified Data.Text as Text
import qualified Data.Map as Map

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
validateAttribute (Name { nameLocalName="cx" }, value) = cx value
validateAttribute (Name { nameLocalName="cy" }, value) = cy value
validateAttribute (Name { nameLocalName="r" }, value) = r value
validateAttribute (Name { nameLocalName="pathLength" }, value) = pathLength value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (name, _) = [InvalidAttribute "circle" name]


cx :: Text.Text -> [Error]
cx "0" = [AttributeDefault "circle" "cx"]
cx _ = []

cy :: Text.Text -> [Error]
cy "0" = [AttributeDefault "circle" "cy"]
cy _ = []

r :: Text.Text -> [Error]
r "0" = [AttributeDefault "circle" "r"]
r _ = []

pathLength :: Text.Text -> [Error]
pathLength "None" = [AttributeDefault "circle" "pathLength"]
pathLength _ = []

fill :: Text.Text -> [Error]
fill "None" = [AttributeDefault "circle" "fill"]
fill _ = []


