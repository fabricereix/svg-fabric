module Svg.Types.Core where




data Length = Length Double deriving (Show, Eq)
data Percentage = Percentage Double deriving (Show, Eq)
data Viewport = Viewport Double Double Double Double deriving (Show, Eq)

data Auto = AUTO deriving (Show, Eq)
data Paint = Color String deriving (Show, Eq)
data Number = Number Double deriving (Show, Eq)

data RemoveFreeze = REMOVE | FREEZE deriving (Show, Eq)




