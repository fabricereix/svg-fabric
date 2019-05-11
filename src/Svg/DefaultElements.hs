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

path :: Element
path = Element {
      elementName=Name { nameLocalName="path", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

polyline :: Element
polyline = Element {
      elementName=Name { nameLocalName="polyline", nameNamespace = Nothing, namePrefix = Nothing}
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

symbol :: Element
symbol = Element {
      elementName=Name { nameLocalName="symbol", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

use :: Element
use = Element {
      elementName=Name { nameLocalName="use", nameNamespace = Nothing, namePrefix = Nothing}
    , elementAttributes=Map.fromList []
    , elementNodes=[]
    }

