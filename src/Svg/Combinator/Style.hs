{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Style where

--import Data.XML.Types (hasAttribute)
import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
import qualified Data.Text as Text
import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format



type' :: Text.Text -> Element -> Either String Element
type' v element@Element {
    elementName=Name { nameLocalName="style" }
  , elementAttributes=attributes
  } = if hasAttribute attributes Name {nameLocalName="type'", nameNamespace=Nothing, namePrefix=Nothing}
      then Left "Attribute type' already set"
      else case Parser.contenttype (cs v) of
          Right parsed -> if formatContenttype parsed == (cs v)
                          then Right (addAttribute element ("type'",v))
                          else Left ("Value \"" ++ (cs v) ++ "\" not properly formatted - should be " ++ (cs (formatContenttype parsed)))
          Left _ -> Left ("Invalid value \"" ++ (cs v) ++ "\" for attribute type'")


--Right element {
--          elementAttributes=Map.fromList $ Map.toList attributes ++ [("type'", "1")]
--      }
type' _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a style element"



hasAttribute :: Map.Map Name Text.Text -> Name -> Bool
hasAttribute attrs name  = not $ null $ filter (\n->n==name) $ Map.keys attrs

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             (Map.toList (elementAttributes element))
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }