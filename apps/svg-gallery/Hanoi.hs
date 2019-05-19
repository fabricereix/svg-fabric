module Hanoi where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Rect as Rect
-- import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import Helper


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [("hanoi.svg", diagram)]


diagram :: Element
diagram = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox 0 0 20 30
                >>= Svg.stroke "black"
                >>= Svg.strokewidth 0.05
                >>= addChildren [
                      hanoi
                    ]

hanoi :: Element
hanoi = fromRight $ Right Default.g
        >>= addChildren stack

stack :: [Element]
stack = [
    fromRight $ Right Default.rect
             >>= Rect.x 5
             >>= Rect.y 2
             >>= Rect.width 1
             >>= Rect.height 8
  , fromRight $ Right Default.rect
             >>= Rect.x 1
             >>= Rect.y 9
             >>= Rect.width 9
             >>= Rect.height 1
             >>= Rect.fill "white"
  , fromRight $ Right Default.rect
             >>= Rect.x 2
             >>= Rect.y 8
             >>= Rect.width 7
             >>= Rect.height 1
             >>= Rect.fill "orange"
  , fromRight $ Right Default.rect
             >>= Rect.x 3
             >>= Rect.y 7
             >>= Rect.width 5
             >>= Rect.height 1
             >>= Rect.fill "blue"
  , fromRight $ Right Default.rect
             >>= Rect.x 4
             >>= Rect.y 6
             >>= Rect.width 3
             >>= Rect.height 1
             >>= Rect.fill "red"
  ]







-- Core
type Disk  = Int
type Stack = [Disk]
type Hanoi = [Stack]

hanoiSequence :: [Hanoi]
hanoiSequence =  [[[0,1,2,3],[],[]],[[1,2,3],[0],[]],[[2,3],[0],[1]],[[2,3],[],[0,1]],[[3],[2],[0,1]],[[0,3],[2],[1]],[[0,3],[1,2],[]],[[3],[0,1,2],[]],[[],[0,1,2],[3]],[[],[1,2],[0,3]],[[1],[2],[0,3]],[[0,1],[2],[3]],[[0,1],[],[2,3]],[[1],[0],[2,3]],[[],[0],[1,2,3]],[[],[],[0,1,2,3]]]



