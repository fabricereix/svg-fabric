{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Circle where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Text.XML


cxLength ::  Double -> Element -> Either String Element
cxLength _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else Right element
cxLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cxPercentage ::  Double -> Element -> Either String Element
cxPercentage _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else Right element
cxPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cyLength ::  Double -> Element -> Either String Element
cyLength _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else Right element
cyLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

cyPercentage ::  Double -> Element -> Either String Element
cyPercentage _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else Right element
cyPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

rLength ::  Double -> Element -> Either String Element
rLength _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else Right element
rLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

rPercentage ::  Double -> Element -> Either String Element
rPercentage _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else Right element
rPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthN ::  Element -> Either String Element
pathLengthN element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthN Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthU ::  Element -> Either String Element
pathLengthU element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthU Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthM ::  Element -> Either String Element
pathLengthM element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthM Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthB ::  Element -> Either String Element
pathLengthB element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthB Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthE ::  Element -> Either String Element
pathLengthE element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthE Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

pathLengthR ::  Element -> Either String Element
pathLengthR element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else Right element
pathLengthR Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke _ element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right element
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a circle instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
