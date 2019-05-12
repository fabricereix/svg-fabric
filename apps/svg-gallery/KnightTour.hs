module KnightTour where

import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import           Text.XML
import qualified Svg.DefaultElements as Default


diagrams :: [(String,Element)]
diagrams = [("knight-tour.svg", diagramSimple)]

diagramSimple :: Element
diagramSimple = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox 0 0 0 0
                >>= addChildren []

-- helpers
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x


-- Core
type Square = (Int, Int)

board :: [Square]
board = [ (x,y) | x <- [0..7], y <- [0..7] ]


tour :: [Square]
tour = undefined



