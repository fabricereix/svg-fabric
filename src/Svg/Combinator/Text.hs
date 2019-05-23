{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Text where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format



x :: Text.Text -> Element -> Either String Element
x v element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute x already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("x",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("x",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute x")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("x", "1")]
--      }
x _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a text element"

y :: Text.Text -> Element -> Either String Element
y v element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="y", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute y already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("y",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("y",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute y")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("y", "1")]
--      }
y _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a text element"

dx :: Text.Text -> Element -> Either String Element
dx v element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dx", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dx already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("dx",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("dx",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute dx")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("dx", "1")]
--      }
dx _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a text element"

dy :: Text.Text -> Element -> Either String Element
dy v element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="dy", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute dy already set"
      else case Parser.length (cs v) of
          Right parsed -> if formatLength parsed == (cs v)
                          then Right (addAttribute element ("dy",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatLength parsed)))
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> if formatPercentage parsed == (cs v)
                              then Right (addAttribute element ("dy",v))
                              else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPercentage parsed)))
              Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute dy")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("dy", "1")]
--      }
dy _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a text element"

class' :: Text.Text -> Element -> Either String Element
class' v element@Element {
    elementName=Name { nameLocalName="text" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="class", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute class already set"
      else case Parser.classes (cs v) of
          Right parsed -> if formatClasses parsed == (cs v)
                          then Right (addAttribute element ("class",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatClasses parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute class")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("class", "1")]
--      }
class' _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a text element"

fill :: Text.Text -> Element -> Either String Element
fill v element@Element {
    elementName=Name { nameLocalName="text" }
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
  } = Left $ (cs name) ++ " element - should be a text element"

stroke :: Text.Text -> Element -> Either String Element
stroke v element@Element {
    elementName=Name { nameLocalName="text" }
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
  } = Left $ (cs name) ++ " element - should be a text element"

strokewidth :: Text.Text -> Element -> Either String Element
strokewidth v element@Element {
    elementName=Name { nameLocalName="text" }
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
  } = Left $ (cs name) ++ " element - should be a text element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }