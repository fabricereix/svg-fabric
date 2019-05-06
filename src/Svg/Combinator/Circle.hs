{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Circle where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML



cx :: String -> Element -> Either String Element
cx _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
cx _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

cy :: String -> Element -> Either String Element
cy _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
cy _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

r :: String -> Element -> Either String Element
r _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
r _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

pathLength :: String -> Element -> Either String Element
pathLength _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
pathLength _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

fill :: String -> Element -> Either String Element
fill _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

stroke :: String -> Element -> Either String Element
stroke _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
