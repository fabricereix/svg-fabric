{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Rect where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML



x :: String -> Element -> Either String Element
x _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
x _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

y :: String -> Element -> Either String Element
y _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
y _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

width :: String -> Element -> Either String Element
width _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
width _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

height :: String -> Element -> Either String Element
height _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
height _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

fill :: String -> Element -> Either String Element
fill _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"

stroke :: String -> Element -> Either String Element
stroke _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a rect element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
