module Svg.Types.Core where




newtype Length = Length Double deriving (Show, Eq)
newtype Percentage = Percentage Double deriving (Show, Eq)
data Viewbox = Viewbox Double Double Double Double deriving (Show, Eq)

data Auto = AUTO deriving (Show, Eq)
newtype Paint = Color String deriving (Show, Eq)
newtype Number = Number Double deriving (Show, Eq)

data RemoveFreeze = REMOVE | FREEZE deriving (Show, Eq)

newtype Points = Points [(Double,Double)] deriving (Show,Eq)

newtype Path = Path [Command]


data Command
  = M Bool Double Double
  | L Bool Double Double
  | H Bool Double
  | V Bool Double
  | Z Bool
  | C Bool Double Double Double Double Double Double
  | S Bool Double Double Double Double
  | Q Bool Double Double Double Double
  | T Bool Double Double
  | A Bool Double Double Double Double Double Double Double
  deriving (Show,Eq)


