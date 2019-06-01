{-# LANGUAGE OverloadedStrings     #-}
module Svg.Attributes.Animate where

all :: [String]
all = [
    "fill"
  ]


defaultValue :: String -> Maybe String
defaultValue "fill" =  Nothing
defaultValue name = error $ "invalid attribute " ++ name ++ " for element animate"


