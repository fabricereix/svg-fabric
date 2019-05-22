{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Circle where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format



cx :: Text.Text -> Element -> Either String Element
cx v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cx already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("cx",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("cx",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute cx")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("cx", "1")]
--      }
cx _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

cy :: Text.Text -> Element -> Either String Element
cy v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute cy already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("cy",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("cy",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute cy")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("cy", "1")]
--      }
cy _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

r :: Text.Text -> Element -> Either String Element
r v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="r", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute r already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("r",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("r",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute r")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("r", "1")]
--      }
r _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

pathLength :: Text.Text -> Element -> Either String Element
pathLength v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="pathLength", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute pathLength already set"
      else case Parser.number (cs v) of
          Right parsed -> if formatNumber parsed == (cs v)
                          then Right (addAttribute element ("pathLength",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatNumber parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute pathLength")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("pathLength", "1")]
--      }
pathLength _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

fill :: Text.Text -> Element -> Either String Element
fill v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="fill", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute fill already set"
      else case Parser.paint (cs v) of
          Right parsed -> if formatPaint parsed == (cs v)
                          then Right (addAttribute element ("fill",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPaint parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute fill")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("fill", "1")]
--      }
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

id :: Text.Text -> Element -> Either String Element
id v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="id", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute id already set"
      else case Parser.id (cs v) of
          Right parsed -> if formatId parsed == (cs v)
                          then Right (addAttribute element ("id",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatId parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute id")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("id", "1")]
--      }
id _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

stroke :: Text.Text -> Element -> Either String Element
stroke v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke already set"
      else case Parser.paint (cs v) of
          Right parsed -> if formatPaint parsed == (cs v)
                          then Right (addAttribute element ("stroke",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPaint parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute stroke")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("stroke", "1")]
--      }
stroke _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"

strokewidth :: Text.Text -> Element -> Either String Element
strokewidth v element@Element {
    elementName=Name { nameLocalName="circle" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="stroke-width", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute stroke-width already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("stroke-width",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("stroke-width",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute stroke-width")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("stroke-width", "1")]
--      }
strokewidth _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a circle element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }