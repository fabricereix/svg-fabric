{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Svg where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML



width ::  Double -> Element -> Either String Element
width a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right $ addAttribute element ("width",cs $ formatLength (Length a0))
width _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

widthPixel ::  Double -> Element -> Either String Element
widthPixel a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else Right $ addAttribute element ("width",cs $ formatPixel (Pixel a0))
widthPixel _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

height ::  Double -> Element -> Either String Element
height a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right $ addAttribute element ("height",cs $ formatLength (Length a0))
height _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

heightPixel ::  Double -> Element -> Either String Element
heightPixel a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else Right $ addAttribute element ("height",cs $ formatPixel (Pixel a0))
heightPixel _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

viewBox ::  Double -> Double -> Double -> Double -> Element -> Either String Element
viewBox a0 a1 a2 a3 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="viewBox", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute viewBox already set"
      else Right $ addAttribute element ("viewBox",cs $ formatViewbox (Viewbox a0 a1 a2 a3))
viewBox _ _ _ _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

strokewidth ::  Double -> Element -> Either String Element
strokewidth a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatLength (Length a0))
strokewidth _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name

strokewidthPercentage ::  Double -> Element -> Either String Element
strokewidthPercentage a0 element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatPercentage (Percentage a0))
strokewidthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a svg instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
