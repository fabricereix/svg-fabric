{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator2.Svg where
import Text.XML
import Data.String.Conversions


width :: String -> Element -> Either String Element
width _ Element {
    elementName=Name { nameLocalName="svg" }
  } = undefined
width _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"

height :: String -> Element -> Either String Element
height _ Element {
    elementName=Name { nameLocalName="svg" }
  } = undefined
height _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"

viewport :: String -> Element -> Either String Element
viewport _ Element {
    elementName=Name { nameLocalName="svg" }
  } = undefined
viewport _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"



