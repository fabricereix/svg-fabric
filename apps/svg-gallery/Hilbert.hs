module Hilbert where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Polyline as Polyline
import qualified Svg.Setter.Rect as Rect
import           Svg.Setter


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [("hilbert.svg",
  fromRight $ Right Default.svg
                >>= Svg.width 300
                >>= Svg.height 300
                >>= Svg.viewBox 0 0 1 1
                >>= addChildren [
                      fromRight $ Right Default.rect
                                   >>= Rect.fill "yellow"
                                   >>= Rect.width 1
                                   >>= Rect.height 1
                    , fromRight $ Right Default.polyline
                                   >>= Polyline.points (map toDoublePoint $ hilbert 1)
                    ]
  )
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
hilbert _ = undefined


