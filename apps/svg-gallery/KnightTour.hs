module KnightTour where

import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Polyline as Polyline
import qualified Svg.Setter.Path as Path
import           Svg.Types.Core
import           Text.XML
import qualified Svg.DefaultElements as Default
import Helper
--import           Data.List                      (minimumBy, (\\))
--import           Data.Ord                       (comparing)


diagrams :: [(String,Element)]
diagrams = [
             ("knight-tour-polyline.svg", diagramSimple tourPolyline)
           , ("knight-tour-path.svg", diagramSimple tourPath)
           ]

diagramSimple :: Element -> Element
diagramSimple tour = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox 0 0 8 8
                >>= addChildren (chessboard++ [tour])


chessboard :: [Element]
chessboard = [cell i j | i<-[0..7],j<-[0..7]]

tourPolyline :: Element
tourPolyline = fromRight $ Right Default.polyline
  >>= Polyline.fill "none"
  >>= Polyline.strokewidth 0.1
  >>= Polyline.stroke "black"
  >>= Polyline.points (map toDoublePoint solution)
  -- >>= Polyline.transform ""


tourPath :: Element
tourPath = fromRight $ Right Default.path
  >>= Path.strokewidth 0.1
  >>= Path.stroke "black"
  >>= Path.fill "none"
  >>= Path.d (M False 0.5 0.5:map toPathCommand (diffPoints solution)++[Z True])
  -- points (map toDoublePoint solution)
  -- >>= Polyline.transform ""



cell :: Int -> Int -> Element
cell i j = fromRight $ Right Default.rect
         >>= Rect.width 1
         >>= Rect.height 1
         >>= Rect.x (fromIntegral i)
         >>= Rect.y (fromIntegral j)
         >>= Rect.fill (if even (i+j) then "#ffce9e" else "#d18b47")




-- Core from https://wiki.haskell.org/The_Knights_Tour
-- complicated
-- even use Control.Monad
-- for the timebeing, use the solution
-- Data.Ord> map fst $ sortBy (comparing snd) $ Map.toList m
solution :: [(Int,Int)]
solution = [(0,0),(1,2),(2,0),(0,1),(1,3),(0,5),(1,7),(3,6),(5,7),(7,6),(6,4),(7,2),(6,0),(4,1),(6,2),(7,0),(5,1),(3,0),(1,1),(0,3),(1,5),(0,7),(2,6),(4,7),(6,6),(7,4),(5,5),(6,7),(7,5),(6,3),(7,1),(5,0),(3,1),(1,0),(2,2),(4,3),(2,4),(4,5),(3,7),(1,6),(0,4),(2,5),(0,6),(2,7),(4,6),(3,4),(5,3),(3,2),(4,0),(6,1),(7,3),(6,5),(7,7),(5,6),(4,4),(5,2),(3,3),(5,4),(3,5),(1,4),(0,2),(2,3),(4,2),(2,1)]


