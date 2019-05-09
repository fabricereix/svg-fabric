module Hilbert where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Polyline as Polyline
import           Svg.Setter


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = map (\n->("hilbert-" ++ show n ++ ".svg", diagram n)) [1..8]


diagram :: Int -> Element
diagram n = fromRight $ Right Default.svg
                >>= Svg.width 300
                >>= Svg.height 300
                >>= Svg.viewBox 0 0 (2^n-1) (2^n-1)
                >>= addChildren [
                      fromRight $ Right Default.polyline
                                   >>= Polyline.fill "white"
                                   >>= Polyline.strokewidth 0.05
                                   >>= Polyline.stroke "red"
                                   >>= Polyline.points (map toDoublePoint $ hilbert n)
                      ]

-- Helpers
type Point = (Int,Int)
toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x,y) = (fromIntegral x, fromIntegral y)

fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x


-- Core
hilbert :: Int -> [Point]
hilbert 1 = [(0,0),(0,1),(1,1),(1,0)]
hilbert n =  map (\(x,y)->(y,m-x)) (reverseList $ hilbert (n-1))
          ++ map (\(x,y)->(x,y+m+1)) (hilbert (n-1))
          ++ map (\(x,y)->(x+m+1,y+m+1)) (hilbert (n-1))
          ++ map (\(x,y)->(2*m+1-y,x)) (reverseList $ hilbert (n-1))
   where m = 2^(n-1)-1

reverseList :: [a] -> [a]
reverseList  [] = []
reverseList  xs = last xs : reverseList (init xs)

part1 :: [Point]->[Point]
part1 = reverseList



test_1 :: IO()
test_1 = do
  putStrLn "hilbert1"
  putStrLn "hilbert2"




