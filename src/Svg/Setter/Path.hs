{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter.Path where
--import Svg.Elements
--import Svg.Types.Core
-- import Svg.Types.Core
import qualified Data.Map as Map
import           Data.String.Conversions
import qualified Data.Text as Text
import           Svg.Types.Core
import           Svg.Types.Format
import           Text.XML
import Svg.Types.Core (Command)


d ::  [Command] -> Element -> Either String Element
d a0 element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="d", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute d already set"
      else Right $ addAttribute element ("d",cs $ formatPath (Path a0))
d _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a path instead of " ++ cs name

fill ::  String -> Element -> Either String Element
fill a0 element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else Right $ addAttribute element ("fill",cs $ formatPaint (Color a0))
fill _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a path instead of " ++ cs name

stroke ::  String -> Element -> Either String Element
stroke a0 element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else Right $ addAttribute element ("stroke",cs $ formatPaint (Color a0))
stroke _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a path instead of " ++ cs name

strokewidth ::  Double -> Element -> Either String Element
strokewidth a0 element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatLength (Length a0))
strokewidth _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a path instead of " ++ cs name

strokewidthPercentage ::  Double -> Element -> Either String Element
strokewidthPercentage a0 element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else Right $ addAttribute element ("stroke-width",cs $ formatPercentage (Percentage a0))
strokewidthPercentage _ Element {
  elementName=Name { nameLocalName=name }
  } = Left $ "should be a path instead of " ++ cs name


hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }
