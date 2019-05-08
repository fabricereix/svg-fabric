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
  , elementNodes = []
  }

