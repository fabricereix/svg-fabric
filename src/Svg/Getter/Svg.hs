module Svg.Getter.Svg where
import Svg.Elements
import Svg.Types.Core


width :: Element -> OneOf3 Auto Length Percentage
width (Svg _ v _ _) = v
width _  = error "Element should be Svg!"

height :: Element -> OneOf3 Auto Length Percentage
height (Svg _ _ v _) = v
height _  = error "Element should be Svg!"

viewport :: Element -> Maybe (OneOf1 Viewport)
viewport (Svg _ _ _ v) = v
viewport _  = error "Element should be Svg!"

