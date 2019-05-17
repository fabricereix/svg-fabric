module Sierpinski where

import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Path as Path
import qualified Svg.Setter.Symbol as Symbol
import qualified Svg.Setter.Use as Use
import           Svg.Types.Core
import           Text.XML
import qualified Svg.DefaultElements as Default
import Helper
import Prelude hiding (id)


diagrams :: [(String,Element)]
diagrams = map (\n->("sierpinski-symbol-" ++ show n ++ ".svg", diagram n)) [1,3,5,8]
        ++ map (\n->("sierpinski-path-" ++ show n ++ ".svg", diagramPath n)) [1,2,3,5,8]

diagram :: Int -> Element
diagram n = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) (2^n+2) (2^n+2)
                >>= addChildren (map symbol [1..n] ++ [fromRight $ Right Default.use >>= Use.href ("#symbol" ++ show n)])


diagramPath :: Int -> Element
diagramPath n = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) (2^n+2) (2^n+2)
                >>= addChildren [
                      fromRight $ Right Default.path
                        >>= Path.strokewidth 0.05
                        >>= Path.stroke "black"
                        >>= Path.fill "black"
                        >>= Path.d (M False 0 0:sierpenskiPath n)
                   ]

sierpenskiPath :: Int -> [Command]
sierpenskiPath 1 = [L True 1 (sqrt 3),L True 1 (-sqrt 3), H True (-2)]
sierpenskiPath n = sierpenskiPath (n - 1)
                ++ M True l 0 :sierpenskiPath (n - 1)
                ++ M True (-l/2) (sqrt 3/2*l) :sierpenskiPath (n - 1)
                ++ [M True (-l/2) (-sqrt 3/2*l)]
    where l = fromIntegral (2^(n-1)::Int) :: Double

triangle :: Element
triangle = fromRight $ Right Default.path
  >>= Path.strokewidth 0.05
  >>= Path.stroke "black"
  >>= Path.fill "black"
  >>= Path.d [M False 0 0, L False 1 (sqrt 3),L False 2 0, Z False]


symbol :: Int -> Element
symbol 1 = fromRight $ Right Default.symbol
               >>= Symbol.id "symbol1"
               >>= addChildren [
                      triangle
                   ]
symbol n = fromRight $ Right Default.symbol
    >>= Symbol.id ("symbol" ++ show n)
    >>= addChildren [
         fromRight $ Right Default.use >>= Use.x 0 >>= Use.y 0 >>= Use.href ("#symbol" ++ show (n-1))
       , use (fromIntegral l / 2) 0  ("#symbol" ++ show (n-1))
       , use (fromIntegral l / 4) (sqrt 3/4*fromIntegral l) ("#symbol" ++ show (n-1))
    ]
    where l = 2^n :: Int

use :: Double -> Double -> String -> Element
use x y id = fromRight $ Right Default.use >>= Use.x x >>= Use.y y >>= Use.href id

