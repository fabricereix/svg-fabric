module Svg.Types.Core where

data Length = Length Double
data Percentage = Percentage Double
data Viewport = Viewport Double Double Double Double

data Auto = AUTO
data Paint = Color String
data Number = Number Double

data RemoveFreeze = REMOVE | FREEZE
