module Svg.Setter.Rect where
import Svg.Elements
import Svg.Types.Core


x :: Element -> OneOf2 Length Percentage -> Element
x (Rect a0 _ a2 a3 a4 a5) v  = (Rect a0 v a2 a3 a4 a5)
x _ _  = error "Element should be Rect!"

y :: Element -> OneOf2 Length Percentage -> Element
y (Rect a0 a1 _ a3 a4 a5) v  = (Rect a0 a1 v a3 a4 a5)
y _ _  = error "Element should be Rect!"

width :: Element -> OneOf3 Auto Length Percentage -> Element
width (Rect a0 a1 a2 _ a4 a5) v  = (Rect a0 a1 a2 v a4 a5)
width _ _  = error "Element should be Rect!"

height :: Element -> OneOf3 Auto Length Percentage -> Element
height (Rect a0 a1 a2 a3 _ a5) v  = (Rect a0 a1 a2 a3 v a5)
height _ _  = error "Element should be Rect!"

fill :: Element -> Maybe (OneOf1 Paint) -> Element
fill (Rect a0 a1 a2 a3 a4 _) v  = (Rect a0 a1 a2 a3 a4 v)
fill _ _  = error "Element should be Rect!"

