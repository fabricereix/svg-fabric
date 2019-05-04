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
  } = Animate.validateAttributes attributes
validate Element {
    elementName=Name {
        nameLocalName="circle"
    }
  , elementAttributes=attributes
  } = Circle.validateAttributes attributes
validate Element {
    elementName=Name {
        nameLocalName="rect"
    }
  , elementAttributes=attributes
  } = Rect.validateAttributes attributes
validate Element {
    elementName=Name {
        nameLocalName="svg"
    }
  , elementAttributes=attributes
  } = Svg.validateAttributes attributes

validate Element {elementName=elemName }  = [InvalidElement elemName]