{-# LANGUAGE OverloadedStrings     #-}
module Svg.Validator where

import Text.XML
--import Data.XML.Types
import Svg.Validator.Core

import qualified Svg.Validator.Animate as Animate
import qualified Svg.Validator.Circle as Circle
import qualified Svg.Validator.Rect as Rect
import qualified Svg.Validator.Svg as Svg



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


validate Element {elementName=elemName }  = [InvalidElement elemName]


validateChildren :: [Node] -> [Error]
validateChildren [] = []
validateChildren (x:xs) = case x of
    NodeElement element -> validate element
    _  -> []
  ++ validateChildren xs



