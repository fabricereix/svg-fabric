{-# LANGUAGE OverloadedStrings #-}
module Test.Svg.Sample where

import qualified Data.Map as Map
import           Text.XML

-- Sample W3C
-- <svg width="100" height="100">
--  <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
-- </svg>
w3c = Element {
    elementName = Name {nameLocalName = "svg", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [
      (Name {nameLocalName = "width", nameNamespace = Nothing, namePrefix = Nothing},"100")
    , (Name {nameLocalName = "height", nameNamespace = Nothing, namePrefix = Nothing},"100")
    ]
  , elementNodes = [
      NodeElement $ Element {
        elementName = Name {nameLocalName = "circle", nameNamespace = Nothing, namePrefix = Nothing}
      , elementAttributes = Map.fromList [
          (Name {nameLocalName = "cx", nameNamespace = Nothing, namePrefix = Nothing},"50")
        , (Name {nameLocalName = "cy", nameNamespace = Nothing, namePrefix = Nothing},"50")
        , (Name {nameLocalName = "stroke", nameNamespace = Nothing, namePrefix = Nothing},"green")
        , (Name {nameLocalName = "stroke-width", nameNamespace = Nothing, namePrefix = Nothing},"4")
        , (Name {nameLocalName = "fill", nameNamespace = Nothing, namePrefix = Nothing},"yellow")
        ]
      , elementNodes = []
      }
    ]
  }


-- <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
--  <path fill="none" stroke="red"
--    d="M 10,30
--       A 20,20 0,0,1 50,30
--       A 20,20 0,0,1 90,30
--       Q 90,60 50,90
--       Q 10,60 10,30 z" />
-- </svg>
heart = Element {
    elementName = Name {nameLocalName = "svg", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [
      (Name {nameLocalName = "viewBox", nameNamespace = Nothing, namePrefix = Nothing},"0 0 100 100")
    ]
  , elementNodes = [
      NodeElement $ Element {
        elementName = Name {nameLocalName = "path", nameNamespace = Nothing, namePrefix = Nothing}
      , elementAttributes = Map.fromList [
          (Name {nameLocalName = "fill", nameNamespace = Nothing, namePrefix = Nothing},"none")
        , (Name {nameLocalName = "stroke", nameNamespace = Nothing, namePrefix = Nothing},"red")
        , (Name {nameLocalName = "d", nameNamespace = Nothing, namePrefix = Nothing}
            ,"M10,30 A20,20 0,0,1 50,30 A20,20 0,0,1 90,30 Q90,60 50,90 Q10,60 10,30 z")
        ]
      , elementNodes = []
      }
    ]
  }

