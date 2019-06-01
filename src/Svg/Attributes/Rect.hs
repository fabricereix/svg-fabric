{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Rect where

all :: [String]
all = [
    "x"
  , "y"
  , "width"
  , "height"
  , "fill"
  , "stroke"
  , "stroke-width"
  ]


defaultValue :: String -> Maybe String
defaultValue "x" = Just "0"
defaultValue "y" = Just "0"
defaultValue "width" =  Nothing
defaultValue "height" =  Nothing
defaultValue "fill" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element rect"


