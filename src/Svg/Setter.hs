{-# LANGUAGE OverloadedStrings     #-}
module Svg.Setter where

import Text.XML
import qualified Data.Text as T
import Svg.Types.Core
import qualified Data.Map as Map
import Svg.Types.Format
import Data.String.Conversions


addChildren :: [Element] -> Element -> Either String Element
addChildren elems Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = Right $ Element {
            elementName=name
          , elementAttributes=attributes
          , elementNodes=children ++ map NodeElement elems
          }



addText :: T.Text -> Element -> Either String Element
addText t Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = Right $ Element {
            elementName=name
          , elementAttributes=attributes
          , elementNodes=children ++ [NodeContent t]
          }


addTransform :: [BasicTransform] -> Element -> Element
addTransform transforms Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = Element {
            elementName=name
          , elementAttributes=Map.fromList $
             Map.toList attributes
             ++ [(Name {nameLocalName="transform", nameNamespace=Nothing, namePrefix=Nothing}, cs $ formatTransform (Transform transforms))]
          , elementNodes=children
          }


