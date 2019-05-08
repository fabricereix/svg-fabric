{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Circle where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML


cxLength ::  Double -> Element -> Either String Element
cxLength a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else Right $ addAttribute element ("cx",cs $ formatLength (Length a0))
cxLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cxPercentage ::  Double -> Element -> Either String Element
cxPercentage a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else Right $ addAttribute element ("cx",cs $ formatPercentage (Percentage a0))
cxPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cyLength ::  Double -> Element -> Either String Element
cyLength a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else Right $ addAttribute element ("cy",cs $ formatLength (Length a0))
cyLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cyPercentage ::  Double -> Element -> Either String Element
cyPercentage a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else Right $ addAttribute element ("cy",cs $ formatPercentage (Percentage a0))
cyPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

rLength ::  Double -> Element -> Either String Element
rLength a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else Right $ addAttribute element ("r",cs $ formatLength (Length a0))
rLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

rPercentage ::  Double -> Element -> Either String Element
rPercentage a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else Right $ addAttribute element ("r",cs $ formatPercentage (Percentage a0))
rPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLength ::  Double -> Element -> Either String Element
pathLength a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right $ addAttribute element ("pathLength",cs $ formatNumber (Number a0))
pathLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

strokewidthLength ::  Double -> Element -> Either String Element
strokewidthLength a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatLength (Length a0))
strokewidthLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

strokewidthPercentage ::  Double -> Element -> Either String Element
strokewidthPercentage a0 element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatPercentage (Percentage a0))
strokewidthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
