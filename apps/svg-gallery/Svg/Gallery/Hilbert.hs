module Svg.Gallery.Hilbert where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Polyline as Polyline
import qualified Svg.Setter.Path as Path
import           Svg.Setter
import           Svg.Types.Core
import Svg.Gallery.Helper


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


-- Core
hilbert :: Int -> [Point]
hilbert 1 = [(0,0),(0,1),(1,1),(1,0)]
hilbert n =  map (\(x,y)->(y,m-x)) (reverseList $ hilbert (n-1))
          ++ map (\(x,y)->(x,y+m+1)) (hilbert (n-1))
          ++ map (\(x,y)->(x+m+1,y+m+1)) (hilbert (n-1))
          ++ map (\(x,y)->(2*m+1-y,x)) (reverseList $ hilbert (n-1))
   where m = 2^(n-1)-1


