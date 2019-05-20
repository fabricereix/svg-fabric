module Svg.Gallery.Helper where

import           Svg.Types.Core

fromRight :: Show l => Either l r -> r
fromRight (Left e)  = error $ show e
fromRight (Right x) = x

reverseList :: [a] -> [a]
reverseList  [] = []
reverseList  xs = last xs : reverseList (init xs)

type Point = (Int,Int)

diffPoints :: [Point]->[(Int,Int)]
diffPoints []                   = []
diffPoints [_]                  = []
diffPoints ((x1,y1):(x2,y2):ps) = (x2-x1,y2-y1):diffPoints ((x2,y2):ps)

toPathCommand :: (Int,Int) -> Command
toPathCommand (0,y) = V True (fromIntegral y)
toPathCommand (x,0) = H True (fromIntegral x)
toPathCommand (x,y) = L True (fromIntegral x) (fromIntegral y)

toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x,y) = (0.5+fromIntegral x , 0.5+fromIntegral y )

optimizePath :: [Command] -> [Command]
optimizePath [] = []
optimizePath [c] = [c]
optimizePath (c1:c2:cs) = case mergeCommands c1 c2 of
   [_,_] -> c1:optimizePath (c2:cs)
   c     -> optimizePath (c ++ cs)


mergeCommands :: Command -> Command -> [Command]
mergeCommands c1@(H True x1) c2@(H True x2) = if signum x1 == signum x2
                                              then [H True (x1+x2)]
                                              else [c1,c2]
mergeCommands c1@(V True x1) c2@(V True x2) = if signum x1 == signum x2
                                              then [V True (x1+x2)]
                                              else [c1,c2]

mergeCommands (M True x1 y1) (M True x2 y2) = [M True (x1+x2) (y1+y2)]
mergeCommands c1 c2 = [c1, c2]


roundPath :: Int -> [Command] -> [Command]
roundPath n = map (roundCommand n)

roundCommand :: Int -> Command -> Command
roundCommand n (L True x y) = L True (roundDouble n x) (roundDouble n y)
roundCommand n (M True x y) = M True (roundDouble n x) (roundDouble n y)
roundCommand _ c            = c

roundDouble :: Int -> Double -> Double
roundDouble n d = fromInteger (round $ d * (10^n)) / (10.0^^n)

