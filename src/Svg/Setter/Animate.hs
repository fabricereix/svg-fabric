{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Animate where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Text.XML


fillREMOVE ::  Element -> Either String Element
fillREMOVE element@Element {
    elementName=Name { nameLocalName="animate" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element
fillREMOVE Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a animate instead of " ++ cs name

fillFREEZE ::  Element -> Either String Element
fillFREEZE element@Element {
    elementName=Name { nameLocalName="animate" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element
fillFREEZE Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a animate instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
