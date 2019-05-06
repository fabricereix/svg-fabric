{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator2.Rect where
import Text.XML
import Data.String.Conversions


x :: String -> Element -> Either String Element
x _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
x _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"

y :: String -> Element -> Either String Element
y _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
y _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"

width :: String -> Element -> Either String Element
width _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
width _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"

height :: String -> Element -> Either String Element
height _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
height _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"

fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"

stroke :: String -> Element -> Either String Element
stroke _ Element {
    elementName=Name { nameLocalName="rect" }
  } = undefined
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = error $ (cs name) ++ " element - should be a rect element"



