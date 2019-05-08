{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Svg where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format



width :: Text.Text -> Element -> Either String Element
width v element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute width already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("width",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute width")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("width", "1")]
--      }
width _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"

height :: Text.Text -> Element -> Either String Element
height v element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="height", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute height already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("height",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute height")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("height", "1")]
--      }
height _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"

viewBox :: Text.Text -> Element -> Either String Element
viewBox v element@Element {
    elementName=Name { nameLocalName="svg" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="viewBox", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute viewBox already set"
      else case Parser.viewbox (cs v) of
          Right parsed -> if formatViewbox parsed == (cs v)
                          then Right (addAttribute element ("viewBox",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatViewbox parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute viewBox")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("viewBox", "1")]
--      }
viewBox _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a svg element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }