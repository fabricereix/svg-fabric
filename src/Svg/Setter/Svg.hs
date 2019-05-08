{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Svg where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Text.XML


width ::  Double -> Element -> Either String Element
width _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element
width _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

height ::  Double -> Element -> Either String Element
height _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element
height _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

viewport ::  Double -> Double -> Double -> Double -> Element -> Either String Element
viewport _ _ _ _ element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="viewport", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute viewport already set"
      else Right element
viewport _ _ _ _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
