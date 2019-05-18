{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.G where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML



fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="g" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a g instead of " ++ cs name

transform ::  [BasicTransform] -> Element -> Either String Element
transform a0 element@Element {
    elementName=Name { nameLocalName="g" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="transform", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute transform already set"
      else Right $ addAttribute element ("transform",cs $ formatTransform (Transform a0))
transform _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a g instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
