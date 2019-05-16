module Helper where

import           Svg.Types.Core

fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x

reverseList :: [a] -> [a]
reverseList  [] = []
reverseList  xs = last xs : reverseList (init xs)

type Point = (Int,Int)

diffPoints :: [Point]->[(Int,Int)]
diffPoints [] = []
diffPoints [_] = []
diffPoints ((x1,y1):(x2,y2):ps) = (x2-x1,y2-y1):diffPoints ((x2,y2):ps)

toPathCommand :: (Int,Int) -> Command
toPathCommand (0,y) = V True (fromIntegral y)
toPathCommand (x,0) = H True (fromIntegral x)
toPathCommand (x,y) = L True (fromIntegral x) (fromIntegral y)

toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x,y) = (0.5+fromIntegral x , 0.5+fromIntegral y )

