{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator where

import Text.XML
--import Data.XML.Types
import Svg.Validator.Core

import qualified Svg.Validator.Animate as Animate
import qualified Svg.Validator.Circle as Circle
import qualified Svg.Validator.G as G
import qualified Svg.Validator.Path as Path
import qualified Svg.Validator.Polyline as Polyline
import qualified Svg.Validator.Rect as Rect
import qualified Svg.Validator.Svg as Svg
import qualified Svg.Validator.Symbol as Symbol
import qualified Svg.Validator.Use as Use



validate :: Element -> [Error]
validate Element {
    elementName=Name {
        nameLocalName="animate"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Animate.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="circle"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Circle.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="g"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = G.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="path"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Path.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="polyline"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Polyline.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="rect"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Rect.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="svg"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Svg.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="symbol"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Symbol.validateAttributes attributes ++ validateChildren children

validate Element {
    elementName=Name {
        nameLocalName="use"
    }
  , elementAttributes=attributes
  , elementNodes=children
  } = Use.validateAttributes attributes ++ validateChildren children


validate Element {elementName=elemName }  = [InvalidElement elemName]


validateChildren :: [Node] -> [Error]
validateChildren [] = []
validateChildren (x:xs) = case x of
    NodeElement element -> validate element
    _  -> []
  ++ validateChildren xs



