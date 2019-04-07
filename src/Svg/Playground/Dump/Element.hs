module Svg.Playground.Dump.Element where

import Svg.Playground.Elements
import qualified Svg.Playground.Dump.Rect as Rect
import qualified Svg.Playground.Dump.Animate as Animate
import Data.Maybe

attributes :: Element -> [(String, String)]
attributes element@(Rect {}) = maybeToList (Rect.x element)
                             ++ maybeToList (Rect.fill element)
                             ++ []

attributes element@(Animate {}) = maybeToList (Animate.fill element)
                             ++ []