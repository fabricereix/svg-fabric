{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Rect where
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
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right $ addAttribute element ("x",cs $ formatLength (Length a0))
x _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

xPercentage ::  Double -> Element -> Either String Element
xPercentage a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else Right $ addAttribute element ("x",cs $ formatPercentage (Percentage a0))
xPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

y ::  Double -> Element -> Either String Element
y a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right $ addAttribute element ("y",cs $ formatLength (Length a0))
y _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

yPercentage ::  Double -> Element -> Either String Element
yPercentage a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else Right $ addAttribute element ("y",cs $ formatPercentage (Percentage a0))
yPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

width ::  Double -> Element -> Either String Element
width a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right $ addAttribute element ("width",cs $ formatLength (Length a0))
width _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

widthPercentage ::  Double -> Element -> Either String Element
widthPercentage a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right $ addAttribute element ("width",cs $ formatPercentage (Percentage a0))
widthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

widthAUTO ::  Element -> Either String Element
widthAUTO element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right $ addAttribute element ("width",cs $ formatAuto (AUTO))
widthAUTO Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

height ::  Double -> Element -> Either String Element
height a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right $ addAttribute element ("height",cs $ formatLength (Length a0))
height _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

heightPercentage ::  Double -> Element -> Either String Element
heightPercentage a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right $ addAttribute element ("height",cs $ formatPercentage (Percentage a0))
heightPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

heightAUTO ::  Element -> Either String Element
heightAUTO element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right $ addAttribute element ("height",cs $ formatAuto (AUTO))
heightAUTO Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

strokewidth ::  Double -> Element -> Either String Element
strokewidth a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatLength (Length a0))
strokewidth _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name

strokewidthPercentage ::  Double -> Element -> Either String Element
strokewidthPercentage a0 element@Element {
    elementName=Name { nameLocalName="rect" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatPercentage (Percentage a0))
strokewidthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a rect instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
