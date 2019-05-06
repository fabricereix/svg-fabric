{-# LANGUAGE OverloadedStrings     #-}
module Svg.DefaultElements where

import qualified Data.Map as Map
import           Text.XML

animate :: Element
animate = Element {
      elementName=Name { nameLocalName="animate", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

circle :: Element
circle = Element {
      elementName=Name { nameLocalName="circle", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

rect :: Element
rect = Element {
      elementName=Name { nameLocalName="rect", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

svg :: Element
svg = Element {
      elementName=Name { nameLocalName="svg", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

