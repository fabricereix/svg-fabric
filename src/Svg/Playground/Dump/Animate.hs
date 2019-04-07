module Svg.Playground.Dump.Animate where

import Svg.Playground.Elements
import qualified Svg.Playground.Getter.Animate as Animate
import Svg.Types.Format



fill :: Element -> Maybe (String,String)
fill element =  if Animate.fill element == Animate.fill defaultAnimate then Nothing
                else case Animate.fill element of
                            (OneOf1 value) -> Just ("fill", formatRemoveFreeze value)


