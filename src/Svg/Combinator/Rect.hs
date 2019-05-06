{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Rect where
import Text.XML
import Data.String.Conversions
import qualified Svg.DefaultElements as Default


x :: String -> Element -> Either String Element
x _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
x _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

y :: String -> Element -> Either String Element
y _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
y _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

width :: String -> Element -> Either String Element
width _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
width _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

height :: String -> Element -> Either String Element
height _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
height _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

stroke :: String -> Element -> Either String Element
stroke _ Element {
    elementName=Name { nameLocalName="rect" }
  } = Right Default.rect
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"



