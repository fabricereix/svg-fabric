{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Polyline where

all :: [String]
all = [
    "points"
  , "fill"
  , "stroke"
  , "stroke-width"
  ]


defaultValue :: String -> Maybe String
defaultValue "points" = Just "None"
defaultValue "fill" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element polyline"


