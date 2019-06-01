{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Text where

all :: [String]
all = [
    "x"
  , "y"
  , "dx"
  , "dy"
  , "class"
  , "fill"
  , "stroke"
  , "stroke-width"
  ]


defaultValue :: String -> Maybe String
defaultValue "x" = Just "0"
defaultValue "y" = Just "0"
defaultValue "dx" = Just "None"
defaultValue "dy" = Just "None"
defaultValue "class" = Just "None"
defaultValue "fill" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element text"


