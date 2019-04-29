module Svg.Setter.Circle where
import Svg.Elements
import Svg.Types.Core


cx :: Element -> OneOf2 Length Percentage -> Element
cx (Circle a0 _ a2 a3 a4 a5) v  = (Circle a0 v a2 a3 a4 a5)
cx _ _  = error "Element should be Circle!"

cy :: Element -> OneOf2 Length Percentage -> Element
cy (Circle a0 a1 _ a3 a4 a5) v  = (Circle a0 a1 v a3 a4 a5)
cy _ _  = error "Element should be Circle!"

r :: Element -> OneOf2 Length Percentage -> Element
r (Circle a0 a1 a2 _ a4 a5) v  = (Circle a0 a1 a2 v a4 a5)
r _ _  = error "Element should be Circle!"

pathLength :: Element -> Maybe (OneOf1 Number) -> Element
pathLength (Circle a0 a1 a2 a3 _ a5) v  = (Circle a0 a1 a2 a3 v a5)
pathLength _ _  = error "Element should be Circle!"

fill :: Element -> Maybe (OneOf1 Paint) -> Element
fill (Circle a0 a1 a2 a3 a4 _) v  = (Circle a0 a1 a2 a3 a4 v)
fill _ _  = error "Element should be Circle!"

