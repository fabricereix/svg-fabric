{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Path where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format



d :: Text.Text -> Element -> Either String Element
d v element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="d", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute d already set"
      else case Parser.path (cs v) of
          Right parsed -> if formatPath parsed == (cs v)
                          then Right (addAttribute element ("d",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatPath parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute d")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("d", "1")]
--      }
d _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a path element"

class' :: Text.Text -> Element -> Either String Element
class' v element@Element {
    elementName=Name { nameLocalName="path" }
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
  } = Left $ (cs name) ++ " element - should be a path element"

fill :: Text.Text -> Element -> Either String Element
fill v element@Element {
    elementName=Name { nameLocalName="path" }
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
  } = Left $ (cs name) ++ " element - should be a path element"

id :: Text.Text -> Element -> Either String Element
id v element@Element {
    elementName=Name { nameLocalName="path" }
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
  } = Left $ (cs name) ++ " element - should be a path element"

stroke :: Text.Text -> Element -> Either String Element
stroke v element@Element {
    elementName=Name { nameLocalName="path" }
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
  } = Left $ (cs name) ++ " element - should be a path element"

strokewidth :: Text.Text -> Element -> Either String Element
strokewidth v element@Element {
    elementName=Name { nameLocalName="path" }
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
  } = Left $ (cs name) ++ " element - should be a path element"

style :: Text.Text -> Element -> Either String Element
style v element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="style", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute style already set"
      else case Parser.css (cs v) of
          Right parsed -> if formatCss parsed == (cs v)
                          then Right (addAttribute element ("style",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatCss parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute style")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("style", "1")]
--      }
style _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a path element"

transform :: Text.Text -> Element -> Either String Element
transform v element@Element {
    elementName=Name { nameLocalName="path" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="transform", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute transform already set"
      else case Parser.transform (cs v) of
          Right parsed -> if formatTransform parsed == (cs v)
                          then Right (addAttribute element ("transform",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatTransform parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute transform")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("transform", "1")]
--      }
transform _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a path element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }