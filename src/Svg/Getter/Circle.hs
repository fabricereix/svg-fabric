module Svg.Getter.Circle where
import Svg.Elements
import Svg.Types.Core


cx :: Element -> OneOf2 Length Percentage
cx (Circle _ v _ _ _ _ _) = v
cx _  = error "Element should be Circle!"

cy :: Element -> OneOf2 Length Percentage
cy (Circle _ _ v _ _ _ _) = v
cy _  = error "Element should be Circle!"

r :: Element -> OneOf2 Length Percentage
r (Circle _ _ _ v _ _ _) = v
r _  = error "Element should be Circle!"

pathLength :: Element -> Maybe (OneOf1 Number)
pathLength (Circle _ _ _ _ v _ _) = v
pathLength _  = error "Element should be Circle!"

fill :: Element -> Maybe (OneOf1 Paint)
fill (Circle _ _ _ _ _ v _) = v
fill _  = error "Element should be Circle!"

stroke :: Element -> Maybe (OneOf1 Paint)
stroke (Circle _ _ _ _ _ _ v) = v
stroke _  = error "Element should be Circle!"

