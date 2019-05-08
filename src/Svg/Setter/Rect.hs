{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Rect where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Text.XML


xLength ::  Double -> Element -> Either String Element
xLength _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right element
xLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

xPercentage ::  Double -> Element -> Either String Element
xPercentage _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right element
xPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

yLength ::  Double -> Element -> Either String Element
yLength _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right element
yLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

yPercentage ::  Double -> Element -> Either String Element
yPercentage _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right element
yPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

widthAUTO ::  Element -> Either String Element
widthAUTO element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element
widthAUTO Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

widthLength ::  Double -> Element -> Either String Element
widthLength _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element
widthLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

widthPercentage ::  Double -> Element -> Either String Element
widthPercentage _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right element
widthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

heightAUTO ::  Element -> Either String Element
heightAUTO element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element
heightAUTO Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

heightLength ::  Double -> Element -> Either String Element
heightLength _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element
heightLength _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

heightPercentage ::  Double -> Element -> Either String Element
heightPercentage _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right element
heightPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right element
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke _ element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right element
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs
