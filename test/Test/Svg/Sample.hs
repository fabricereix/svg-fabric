{-# LANGUAGE OverloadedStrings #-}
module Test.Svg.Sample where

import qualified Data.Map as Map
import           Text.XML

-- Sample W3C
-- <svg width="100" height="100">
--  <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
-- </svg>
w3c = Element {
    elementName = Name {nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = Map.fromList [(Name {nameLocalName = "fill", nameNamespace = Nothing, namePrefix = Nothing},"black"),(Name {nameLocalName = "x", nameNamespace = Nothing, namePrefix = Nothing},"1")], elementNodes = []}

