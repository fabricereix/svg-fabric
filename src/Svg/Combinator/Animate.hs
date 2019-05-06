{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Animate where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML



fill :: String -> Element -> Either String Element
fill _ element@Element {
    elementName=Name { nameLocalName="animate" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element {
          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
      }
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a animate element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
