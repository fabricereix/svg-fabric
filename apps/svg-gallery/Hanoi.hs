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
                           stacks 10 (stack1,stack2,[])
                         , stacks 20 ([],stack2,[])
                    ]

--hanoi :: Element
--hanoi = fromRight $ Right Default.g
--        >>= addChildren (stacks 10 (stack1,stack2,[]))

createDisk :: Int -> Disk
createDisk 0 = Disk 9 "white"
createDisk 1 = Disk 7 "red"
createDisk 2 = Disk 5 "blue"
createDisk 3 = Disk 3 "green"
createDisk _ = error "invalid disk number"

stack1,stack2 :: [Disk]
stack1 = map createDisk [0,1,2,3]
stack2 = map createDisk [0,3]

type Pos = (Double,Double)

data Disk = Disk {
    size :: Double
  , color :: String
  }



stacks :: Double -> ([Disk],[Disk],[Disk]) -> Element
stacks y (d1,d2,d3) = fromRight $ Right Default.g
        >>= addChildren (
                   stack (8,y) d1
                   ++ stack (20,y) d2
                   ++ stack (30,y) d3
        )

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
hanoiSequence :: [[[Int]]] -- Stacks of Disks
hanoiSequence =  [[[0,1,2,3],[],[]],[[1,2,3],[0],[]],[[2,3],[0],[1]],[[2,3],[],[0,1]],[[3],[2],[0,1]],[[0,3],[2],[1]],[[0,3],[1,2],[]],[[3],[0,1,2],[]],[[],[0,1,2],[3]],[[],[1,2],[0,3]],[[1],[2],[0,3]],[[0,1],[2],[3]],[[0,1],[],[2,3]],[[1],[0],[2,3]],[[],[0],[1,2,3]],[[],[],[0,1,2,3]]]



