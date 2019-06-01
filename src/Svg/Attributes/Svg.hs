{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Svg where

all :: [String]
all = [
    "width"
  , "height"
  , "viewBox"
  , "stroke"
  , "stroke-width"
  ]


defaultValue :: String -> Maybe String
defaultValue "width" = Just "None"
defaultValue "height" = Just "None"
defaultValue "viewBox" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element svg"


