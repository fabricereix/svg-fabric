{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Svg.Xml where

import Svg.Xml.Transform
import Test.Framework
import Text.XML
import qualified Data.Map as Map

import Svg.Types.Core
import Svg.Types.Format
import Svg.Types.Parser
import qualified Data.Text as T
import Prelude hiding (length,id)
import qualified Prelude
import Data.String.Conversions

roundDouble :: Int -> Double -> Double
roundDouble n d = fromInteger (round $ d * (10^n)) / (10.0^^n)

element1 = Element {
    elementName = Name {nameLocalName = "g", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [
      (Name {nameLocalName = "class", nameNamespace = Nothing, namePrefix = Nothing},"symbol")
    ]
  , elementNodes = [
      NodeElement $ Element {
        elementName = Name {nameLocalName = "circle", nameNamespace = Nothing, namePrefix = Nothing}
      , elementAttributes = Map.fromList [
          (Name {nameLocalName = "cx", nameNamespace = Nothing, namePrefix = Nothing},"1")
        , (Name {nameLocalName = "cy", nameNamespace = Nothing, namePrefix = Nothing},"1.111")
        , (Name {nameLocalName = "r", nameNamespace = Nothing, namePrefix = Nothing},"1")
        ]
      , elementNodes = []
      }
    ]
  }


test_mapElements = do
  assertEqual element1 $ mapElements Prelude.id element1
  print $ mapElements (roundCircle 2) element1


roundCircle :: Int -> Element -> Element
roundCircle n Element {
    elementName="circle"
  , elementAttributes=attributes
  , elementNodes=children
  } = Element {
            elementName="circle"
          , elementAttributes=Map.fromList $ map (roundAttributeCircle n) $ Map.toList attributes
          , elementNodes=children
          }
roundCircle _ element = element


roundNodeCircle :: Int -> Node -> Node
roundNodeCircle n (NodeElement element) = NodeElement (roundCircle n element)
roundNodeCircle _ node = node

roundAttributeCircle :: Int -> (Name, T.Text)-> (Name,T.Text)
roundAttributeCircle n (Name {nameLocalName="cx", nameNamespace=Nothing, namePrefix=Nothing}, v) = case length (cs v) of
  Left e-> error $ show e
  Right (Length x) -> ("cx", cs $ formatLength (Length (roundDouble n x)))
roundAttributeCircle n (Name {nameLocalName="cy", nameNamespace=Nothing, namePrefix=Nothing}, v) = case length (cs v) of
  Left e-> error $ show e
  Right (Length x) -> ("cy", cs $ formatLength (Length (roundDouble n x)))
roundAttributeCircle _ a = a
