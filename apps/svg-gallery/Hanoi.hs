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
                >>= Svg.viewBox 0 0 40 40
                >>= Svg.stroke "black"
                >>= Svg.strokewidth 0.05
                >>= addChildren [
                      hanoi
                    ]

hanoi :: Element
hanoi = fromRight $ Right Default.g
        >>= addChildren (stack (5,2) stack1
                      ++ stack (5,12) []
                      ++ stack (25,12) stack2
            )

stack1,stack2 :: [Disk]
stack1 = [Disk 9 "white", Disk 7 "orange", Disk 5 "blue", Disk 3 "red"]
stack2 = [Disk 9 "white", Disk 7 "orange", Disk 5 "blue"]

type Pos = (Double,Double)

data Disk = Disk {
    size :: Double
  , color :: String
  }

stack :: Pos -> [Disk] -> [Element]
stack (x,y) disks =
    fromRight (Right Default.rect
             >>= Rect.x x
             >>= Rect.y y
             >>= Rect.width diskThickness
             >>= Rect.height pegHeight
  ):map (\(i,Disk s c)->fromRight $ Right Default.rect
          >>= Rect.x (x-(s-1)/2)
          >>= Rect.y (y+pegHeight-i)
          >>= Rect.width s
          >>= Rect.height diskThickness
          >>= Rect.fill c
          ) (zip [1..] disks)
   where pegHeight = 8
         diskThickness = 1





-- Core
type Stack = [Int]
type Hanoi = [Stack]

hanoiSequence :: [Hanoi]
hanoiSequence =  [[[0,1,2,3],[],[]],[[1,2,3],[0],[]],[[2,3],[0],[1]],[[2,3],[],[0,1]],[[3],[2],[0,1]],[[0,3],[2],[1]],[[0,3],[1,2],[]],[[3],[0,1,2],[]],[[],[0,1,2],[3]],[[],[1,2],[0,3]],[[1],[2],[0,3]],[[0,1],[2],[3]],[[0,1],[],[2,3]],[[1],[0],[2,3]],[[],[0],[1,2,3]],[[],[],[0,1,2,3]]]



