module Svg.Setter.Svg where
import Svg.Elements
import Svg.Types.Core


width :: Element -> OneOf3 Auto Length Percentage -> Element
width (Svg a0 _ a2 a3) v  = (Svg a0 v a2 a3)
width _ _  = error "Element should be Svg!"

height :: Element -> OneOf3 Auto Length Percentage -> Element
height (Svg a0 a1 _ a3) v  = (Svg a0 a1 v a3)
height _ _  = error "Element should be Svg!"

viewport :: Element -> Maybe (OneOf1 Viewport) -> Element
viewport (Svg a0 a1 a2 _) v  = (Svg a0 a1 a2 v)
viewport _ _  = error "Element should be Svg!"

