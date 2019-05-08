module Svg.Types.Core where




newtype Length = Length Double deriving (Show, Eq)
newtype Percentage = Percentage Double deriving (Show, Eq)
data Viewbox = Viewbox Double Double Double Double deriving (Show, Eq)

data Auto = AUTO deriving (Show, Eq)
newtype Paint = Color String deriving (Show, Eq)
newtype Number = Number Double deriving (Show, Eq)

data RemoveFreeze = REMOVE | FREEZE deriving (Show, Eq)

newtype Points = Points [(Double,Double)] deriving (Show,Eq)



