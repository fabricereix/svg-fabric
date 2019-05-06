{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Svg where
import Text.XML
import Data.String.Conversions
import qualified Svg.DefaultElements as Default


width :: String -> Element -> Either String Element
width _ Element {
    elementName=Name { nameLocalName="svg" }
  } = Right Default.svg
width _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"

height :: String -> Element -> Either String Element
height _ Element {
    elementName=Name { nameLocalName="svg" }
  } = Right Default.svg
height _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"

viewport :: String -> Element -> Either String Element
viewport _ Element {
    elementName=Name { nameLocalName="svg" }
  } = Right Default.svg
viewport _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a svg element"



