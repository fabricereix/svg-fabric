module KnightTour where

import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Polyline as Polyline
import           Text.XML
import qualified Svg.DefaultElements as Default
import           Data.List                      (minimumBy, (\\))
import           Data.Ord                       (comparing)


diagrams :: [(String,Element)]
diagrams = [("knight-tour.svg", diagramSimple)]

diagramSimple :: Element
diagramSimple = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox 0 0 8 8
                >>= addChildren (chessboard++ [tour])


chessboard :: [Element]
chessboard = [cell i j | i<-[0..7],j<-[0..7]]

tour :: Element
tour = fromRight $ Right Default.polyline
  >>= Polyline.fill "none"
  >>= Polyline.strokewidth 0.1
  >>= Polyline.stroke "black"
  >>= Polyline.points (map toDoublePoint solution)
  -- >>= Polyline.transform ""



-- helpers
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x


cell :: Int -> Int -> Element
cell i j = fromRight $ Right Default.rect
         >>= Rect.width 1
         >>= Rect.height 1
         >>= Rect.x (fromIntegral i)
         >>= Rect.y (fromIntegral j)
         >>= Rect.fill (if even (i+j) then "#ffce9e" else "#d18b47")

type Point = (Int,Int)
toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x,y) = (0.5+fromIntegral x , 0.5+fromIntegral y )



-- Core
type Square = (Int, Int)

board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]



knightMoves :: Square -> [Square]
knightMoves (x,y) = filter (`elem` board) jumps
   where jumps = [ (x+i,y+j) | i <- jv, j <- jv, abs i /= abs j ]
         jv    = [1,-1,2,-2]

knightTour :: Square -> [Square]
knightTour sq = knightTour' [sq]
   where
     knightTour' moves@(lastMove:_)
         | null candMoves = reverse moves
         | otherwise = knightTour' $ newSquare : moves
       where newSquare   = minimumBy (comparing (length . findMoves)) candMoves
             candMoves   = findMoves lastMove
             findMoves s = knightMoves s \\ moves
     knightTour' [] = []


{-- closed tour solution
0   47  14  31  62  27  12  29
15  32  63  54  13  30  57  26
48  1   46  61  56  59  28  11
33  16  55  50  53  44  25  58
2   49  42  45  60  51  10  39
17  34  19  52  43  40  7   24
20  3   36  41  22  5   38  9
35  18  21  4   37  8   23  6

soltions given as square coordinates instead
(0,0), (1,2), (0,4), ...
=> more direct mapping to digrams
=> above is a more compact representaion (but less funcdamental?)
assumption start with cell (0,0)
you know that there is a solution
first move to (1,2) and last to (2,1)

backtracking in haskell??

--}

solution :: [(Int,Int)]
solution = [(0,0),(1,2),(3,1),(2,3),(1,1),(3,2)]



