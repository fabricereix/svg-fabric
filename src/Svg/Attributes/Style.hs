{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Style where

all :: [String]
all = [
    "type'"
  ]


defaultValue :: String -> Maybe String
defaultValue "type'" = Just "None"
defaultValue name = error $ "invalid attribute " ++ name ++ " for element style"


