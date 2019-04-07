module Svg.Playground.Xml where

import Svg.Playground.Elements
import qualified Text.XML as XML
--import Text.XML hiding (Element)


decode :: XML.Element -> (Element, [(String,String)])
decode = undefined


encode :: Element -> XML.Element
encode = undefined