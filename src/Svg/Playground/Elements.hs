module Svg.Playground.Elements where

import Svg.Types.Core

data OneOf1 a = OneOf1 a
                deriving (Eq,Show)

data OneOf2 a b = OneOf2 a | TwoOf2 b
                  deriving (Eq,Show)

data OneOf3 a b c = OneOf3 a | TwoOf3 b | ThreeOf3 c
                  deriving (Eq,Show)


data Element = Rect
                 (OneOf2 Length Percentage)   -- x
                 (Maybe (OneOf1 Paint))       -- fill
             | Animate
                 (OneOf1 RemoveFreeze)        -- fill

