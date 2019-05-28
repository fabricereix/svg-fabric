module Svg.Xml.Transform where

import Text.XML

mapElements :: (Element -> Element) -> Element -> Element
mapElements mapper element = (mapper element) {
    elementNodes=map (mapNode mapper) (elementNodes element)
  }


mapNode :: (Element->Element) -> Node -> Node
mapNode mapper (NodeElement element) = NodeElement (mapper element)
mapNode _ node = node
