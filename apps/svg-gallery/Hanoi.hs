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
                >>= Svg.viewBox 0 0 40 (fromIntegral (10 * (1+length hanoiSequence)))
                >>= Svg.stroke "black"
                >>= Svg.strokewidth 0.05
                >>= addChildren (map (\(i,xs)->stateElement (i*10) (map createStack xs)) $ zip [1..] hanoiSequence)
                -- >>= addChildren (map (\(i,xs)->stacks (i*10) (map createDisk s1, map createDisk s2, map createDisk s3)) $ zip [1..] hanoiSequence)
                      --     stacks 10 (stack1,stack2,[])
                      --   , stacks 20 ([],stack2,[])

--hanoi :: Element
--hanoi = fromRight $ Right Default.g
--        >>= addChildren (stacks 10 (stack1,stack2,[]))

createDisk :: Int -> Disk
createDisk 0 = Disk 3 "white"
createDisk 1 = Disk 5 "red"
createDisk 2 = Disk 7 "blue"
createDisk 3 = Disk 9 "green"
createDisk _ = error "invalid disk number"

stack1,stack2 :: Stack
stack1 = createStack [0,1,2,3]
stack2 = createStack [0,3]

createStack :: [Int] -> Stack
createStack = map createDisk

type Pos = (Double,Double)

data Disk = Disk {
    size :: Double
  , color :: String
  }
type Stack = [Disk]
type State = [Stack]


stateElement :: Double -> State -> Element
stateElement y xs = fromRight $ Right Default.g
        >>= addChildren (concatMap (\(i,s)->stackElements (i*10,y) s) $ zip [1..] xs)

stackElements :: Pos -> Stack -> [Element]
stackElements (x,y) disks =
    fromRight (Right Default.rect
             >>= Rect.x x
             >>= Rect.y y
             >>= Rect.width diskThickness
             >>= Rect.height pegHeight
  ):map (\(i,Disk s c)->fromRight $ Right Default.rect
          >>= Rect.x (x-(s-1)/2)
          >>= Rect.y (y+pegHeight+i-fromIntegral (length disks) -1)
          >>= Rect.width s
          >>= Rect.height diskThickness
          >>= Rect.fill c
          ) (zip [1..] disks)
   where pegHeight = 8
         diskThickness = 1





-- Core
hanoiSequence :: [[[Int]]] -- Stacks of Disks
hanoiSequence =  [[[0,1,2,3],[],[]],[[1,2,3],[0],[]],[[2,3],[0],[1]],[[2,3],[],[0,1]],[[3],[2],[0,1]],[[0,3],[2],[1]],[[0,3],[1,2],[]],[[3],[0,1,2],[]],[[],[0,1,2],[3]],[[],[1,2],[0,3]],[[1],[2],[0,3]],[[0,1],[2],[3]],[[0,1],[],[2,3]],[[1],[0],[2,3]],[[],[0],[1,2,3]],[[],[],[0,1,2,3]]]



