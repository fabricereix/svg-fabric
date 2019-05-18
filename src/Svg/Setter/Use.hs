{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Use where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML



href ::  String -> Element -> Either String Element
href a0 element@Element {
    elementName=Name { nameLocalName="use" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="href", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute href already set"
      else Right $ addAttribute element ("href",cs $ formatId (Id a0))
href _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a use instead of " ++ cs name

x ::  Double -> Element -> Either String Element
x a0 element@Element {
    elementName=Name { nameLocalName="use" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right $ addAttribute element ("x",cs $ formatLength (Length a0))
x _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a use instead of " ++ cs name

y ::  Double -> Element -> Either String Element
y a0 element@Element {
    elementName=Name { nameLocalName="use" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right $ addAttribute element ("y",cs $ formatLength (Length a0))
y _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a use instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="use" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a use instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="use" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a use instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
