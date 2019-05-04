module Svg.Getter.Rect where
import Svg.Elements
import Svg.Types.Core


x :: Element -> OneOf2 Length Percentage
x (Rect _ v _ _ _ _ _) = v
x _  = error "Element should be Rect!"

y :: Element -> OneOf2 Length Percentage
y (Rect _ _ v _ _ _ _) = v
y _  = error "Element should be Rect!"

width :: Element -> OneOf3 Auto Length Percentage
width (Rect _ _ _ v _ _ _) = v
width _  = error "Element should be Rect!"

height :: Element -> OneOf3 Auto Length Percentage
height (Rect _ _ _ _ v _ _) = v
height _  = error "Element should be Rect!"

fill :: Element -> Maybe (OneOf1 Paint)
fill (Rect _ _ _ _ _ v _) = v
fill _  = error "Element should be Rect!"

stroke :: Element -> Maybe (OneOf1 Paint)
stroke (Rect _ _ _ _ _ _ v) = v
stroke _  = error "Element should be Rect!"

