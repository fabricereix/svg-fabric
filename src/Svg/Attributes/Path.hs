{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Path where

all :: [String]
all = [
    "d"
  , "class"
  , "fill"
  , "id"
  , "stroke"
  , "stroke-width"
  , "style"
  , "transform"
  ]


defaultValue :: String -> Maybe String
defaultValue "d" = Just "None"
defaultValue "class" = Just "None"
defaultValue "fill" = Just "None"
defaultValue "id" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue "stroke-width" =  Nothing
defaultValue "style" = Just "None"
defaultValue "transform" = Just "None"
defaultValue name = error $ "invalid attribute " ++ name ++ " for element path"


