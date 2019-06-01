{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Circle where

all :: [String]
all = [
    "cx"
  , "cy"
  , "r"
  , "pathLength"
  , "fill"
  , "id"
  , "stroke"
  , "stroke-width"
  ]


defaultValue :: String -> Maybe String
defaultValue "cx" = Just "0"
defaultValue "cy" = Just "0"
defaultValue "r" = Just "0"
defaultValue "pathLength" = Just "None"
defaultValue "fill" = Just "None"
defaultValue "id" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element circle"


