module Svg.Elements where

import Svg.Types.Core


data OneOf1 a = OneOf1 a
                deriving (Eq,Show)

data OneOf2 a b = OneOf2 a | TwoOf2 b
                  deriving (Eq,Show)

data OneOf3 a b c = OneOf3 a | TwoOf3 b | ThreeOf3 c
                  deriving (Eq,Show)


data Element =
    Animate
      (OneOf1 RemoveFreeze) -- fill

  | Circle
      (OneOf2 Length Percentage) -- cx
      (OneOf2 Length Percentage) -- cy
      (OneOf2 Length Percentage) -- r
      (Maybe (OneOf1 Number)) -- pathLength
      (Maybe (OneOf1 Paint)) -- fill

  | Rect
      (OneOf2 Length Percentage) -- x
      (OneOf2 Length Percentage) -- y
      (OneOf3 Auto Length Percentage) -- width
      (OneOf3 Auto Length Percentage) -- height
      (Maybe (OneOf1 Paint)) -- fill

  | Svg
      (OneOf3 Auto Length Percentage) -- width
      (OneOf3 Auto Length Percentage) -- height
      (Maybe (OneOf1 Viewport)) -- viewport


  deriving (Show, Eq)