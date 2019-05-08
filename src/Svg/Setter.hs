module Svg.Setter where

import Text.XML


addChildren :: [Element] -> Element -> Either String Element
addChildren elems (Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  }) = Right $ Element {
            elementName=name
          , elementAttributes=attributes
          , elementNodes=children ++ map NodeElement elems
          }



