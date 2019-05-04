{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator.Rect where

import Svg.Validator.Core
import Text.XML
-- import Data.XML.Types
import qualified Data.Text as Text
import qualified Data.Map as Map

validateAttributes :: Map.Map Name Text.Text -> [Error]
validateAttributes attributes = concatMap validateAttribute $ Map.toList attributes

validateAttribute :: (Name, Text.Text) -> [Error]
validateAttribute (Name { nameLocalName="x" }, value) = x value
validateAttribute (Name { nameLocalName="y" }, value) = y value
validateAttribute (Name { nameLocalName="width" }, value) = width value
validateAttribute (Name { nameLocalName="height" }, value) = height value
validateAttribute (Name { nameLocalName="fill" }, value) = fill value
validateAttribute (name, _) = [InvalidAttribute "rect" name]


x :: Text.Text -> [Error]
x "0" = [AttributeDefault "rect" "x"]
x _ = []

y :: Text.Text -> [Error]
y "0" = [AttributeDefault "rect" "y"]
y _ = []

width :: Text.Text -> [Error]
width "auto" = [AttributeDefault "rect" "width"]
width _ = []

height :: Text.Text -> [Error]
height "auto" = [AttributeDefault "rect" "height"]
height _ = []

fill :: Text.Text -> [Error]
fill "None" = [AttributeDefault "rect" "fill"]
fill _ = []


