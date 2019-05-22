{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Text where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML



x ::  Double -> Element -> Either String Element
x a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right $ addAttribute element ("x",cs $ formatLength (Length a0))
x _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

xPercentage ::  Double -> Element -> Either String Element
xPercentage a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right $ addAttribute element ("x",cs $ formatPercentage (Percentage a0))
xPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

y ::  Double -> Element -> Either String Element
y a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right $ addAttribute element ("y",cs $ formatLength (Length a0))
y _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

yPercentage ::  Double -> Element -> Either String Element
yPercentage a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right $ addAttribute element ("y",cs $ formatPercentage (Percentage a0))
yPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

dx ::  Double -> Element -> Either String Element
dx a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dx already set"
      else Right $ addAttribute element ("dx",cs $ formatLength (Length a0))
dx _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

dxPercentage ::  Double -> Element -> Either String Element
dxPercentage a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dx already set"
      else Right $ addAttribute element ("dx",cs $ formatPercentage (Percentage a0))
dxPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

dy ::  Double -> Element -> Either String Element
dy a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dy already set"
      else Right $ addAttribute element ("dy",cs $ formatLength (Length a0))
dy _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

dyPercentage ::  Double -> Element -> Either String Element
dyPercentage a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dy already set"
      else Right $ addAttribute element ("dy",cs $ formatPercentage (Percentage a0))
dyPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

strokewidth ::  Double -> Element -> Either String Element
strokewidth a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatLength (Length a0))
strokewidth _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name

strokewidthPercentage ::  Double -> Element -> Either String Element
strokewidthPercentage a0 element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatPercentage (Percentage a0))
strokewidthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a text instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
