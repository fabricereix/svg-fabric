{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator2.Circle where
import Text.XML
import Data.String.Conversions


cx :: String -> Element -> Either String Element
cx _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
cx _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"

cy :: String -> Element -> Either String Element
cy _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
cy _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"

r :: String -> Element -> Either String Element
r _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
r _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"

pathLength :: String -> Element -> Either String Element
pathLength _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
pathLength _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"

fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"

stroke :: String -> Element -> Either String Element
stroke _ Element {
    elementName=Name { nameLocalName="circle" }
  } = undefined
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a circle element"



