{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Circle where
import Text.XML
import Data.String.Conversions
import qualified Svg.DefaultElements as Default


cx :: String -> Element -> Either String Element
cx _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
cx _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

cy :: String -> Element -> Either String Element
cy _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
cy _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

r :: String -> Element -> Either String Element
r _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
r _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

pathLength :: String -> Element -> Either String Element
pathLength _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
pathLength _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

stroke :: String -> Element -> Either String Element
stroke _ Element {
    elementName=Name { nameLocalName="circle" }
  } = Right Default.circle
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"



