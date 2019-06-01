{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Use where

all :: [String]
all = [
    "href"
  , "x"
  , "y"
  , "fill"
  , "stroke"
  ]


defaultValue :: String -> Maybe String
defaultValue "href" = Just "None"
defaultValue "x" = Just "None"
defaultValue "y" = Just "None"
defaultValue "fill" = Just "None"
defaultValue "stroke" = Just "None"
defaultValue name = error $ "invalid attribute " ++ name ++ " for element use"


