{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.G where

all :: [String]
all = [
    "fill"
  , "stroke"
  , "stroke-width"
  , "transform"
  ]


defaultValue :: String -> Maybe String
defaultValue "fill" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue "transform" = Just "None"
defaultValue name = error $ "invalid attribute " ++ name ++ " for element g"


