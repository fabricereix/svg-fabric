{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Animate where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML


fill ::  Element -> Either String Element
fill element@Element {
    elementName=Name { nameLocalName="animate" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatRemovefreeze (REMOVE))
fill Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a animate instead of " ++ cs name

fillFREEZE ::  Element -> Either String Element
fillFREEZE element@Element {
    elementName=Name { nameLocalName="animate" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatRemovefreeze (FREEZE))
fillFREEZE Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a animate instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
