module Svg.Setter where

import Text.XML
import qualified Data.Text as T

addChildren :: [Element] -> Element -> Either String Element
addChildren elems Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = Right $ Element {
            elementName=name
          , elementAttributes=attributes
          , elementNodes=children ++ map NodeElement elems
          }



addText :: T.Text -> Element -> Either String Element
addText t Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = Right $ Element {
            elementName=name
          , elementAttributes=attributes
          , elementNodes=children ++ [NodeContent t]
          }

