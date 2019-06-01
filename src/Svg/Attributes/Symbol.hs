{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Symbol where

all :: [String]
all = [
    "width"
  , "height"
  , "viewBox"
  , "id"
  ]


defaultValue :: String -> Maybe String
defaultValue "width" = Just "None"
defaultValue "height" = Just "None"
defaultValue "viewBox" = Just "None"
defaultValue "id" = Just "None"
defaultValue name = error $ "invalid attribute " ++ name ++ " for element symbol"


