{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Style where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML



type' ::  String -> Element -> Either String Element
type' a0 element@Element {
    elementName=Name { nameLocalName="style" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="type'", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute type' already set"
      else Right $ addAttribute element ("type'",cs $ formatContenttype (ContentType a0))
type' _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a style instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
