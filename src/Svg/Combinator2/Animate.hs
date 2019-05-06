{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator2.Animate where
import Text.XML
import Data.String.Conversions


fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="animate" }
  } = undefined
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a animate element"



