module Hilbert where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Polyline as Polyline
import qualified Svg.Setter.Path as Path
import           Svg.Setter
import           Svg.Types.Core


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = concatMap (\n->[
        ("hilbert-polyline-" ++ show n ++ ".svg", diagramPolyline n)
      , ("hilbert-path-" ++ show n ++ ".svg", diagramPath n)
      ]) [1..7]



diagramPolyline :: Int -> Element
diagramPolyline n = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) (2^n+1) (2^n+1)
                >>= addChildren [
                      fromRight $ Right Default.polyline
                                   >>= Polyline.fill "white"
                                   >>= Polyline.strokewidth 0.1
                                   >>= Polyline.stroke "darkred"
                                   >>= Polyline.points (map toDoublePoint $ hilbert n)
                      ]
diagramPath :: Int -> Element
diagramPath n = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) (2^n+1) (2^n+1)
                >>= addChildren [
                      fromRight $ Right Default.path
                                   >>= Path.strokewidth 0.1
                                   >>= Path.stroke "darkred"
                                   >>= Path.fill "none"
                                   >>= Path.d (optimizePath (M False 0 0:map toPathCommand (diffPoints $ hilbert n)))
                      ]

-- Helpers
type Point = (Int,Int)
toDoublePoint :: Point -> (Double, Double)
toDoublePoint (x,y) = (fromIntegral x, fromIntegral y)

toPathCommand :: (Int,Int) -> Command
toPathCommand (0,y) = V True (fromIntegral y)
toPathCommand (x,0) = H True (fromIntegral x)
toPathCommand (x,y) = L True (fromIntegral x) (fromIntegral y)

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




diffPoints :: [Point]->[(Int,Int)]
diffPoints [] = []
diffPoints [_] = []
diffPoints ((x1,y1):(x2,y2):ps) = (x2-x1,y2-y1):diffPoints ((x2,y2):ps)


optimizePath :: [Command] -> [Command]
optimizePath [] = []
optimizePath [c] = [c]
optimizePath (c1:c2:cs) = case mergeCommands c1 c2 of
   [_,_] -> c1:optimizePath (c2:cs)
   c  -> optimizePath (c ++ cs)


mergeCommands :: Command -> Command -> [Command]
mergeCommands c1@(H True x1) c2@(H True x2) = if signum x1 == signum x2
                                              then [H True (x1+x2)]
                                              else [c1,c2]
mergeCommands c1@(V True x1) c2@(V True x2) = if signum x1 == signum x2
                                              then [V True (x1+x2)]
                                              else [c1,c2]
mergeCommands c1 c2 = [c1, c2]



