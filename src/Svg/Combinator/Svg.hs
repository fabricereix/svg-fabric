{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Svg where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML



width :: String -> Element -> Either String Element
width _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
width _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"

height :: String -> Element -> Either String Element
height _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
height _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"

viewport :: String -> Element -> Either String Element
viewport _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="viewport", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute viewport already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
viewport _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
