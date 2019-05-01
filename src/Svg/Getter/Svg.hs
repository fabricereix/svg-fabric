module Svg.Getter.Svg where
import Svg.Elements
import Svg.Types.Core


width :: Element -> Maybe (OneOf1 Length)
width (Svg _ v _ _) = v
width _  = error "Element should be Svg!"

height :: Element -> Maybe (OneOf1 Length)
height (Svg _ _ v _) = v
height _  = error "Element should be Svg!"

viewport :: Element -> Maybe (OneOf1 Viewport)
viewport (Svg _ _ _ v) = v
viewport _  = error "Element should be Svg!"

