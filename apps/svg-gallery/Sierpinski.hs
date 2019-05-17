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


diagrams :: [(String,Element)]
diagrams = [
             ("sierpinsky-3.svg", diagram 3)
           , ("sierpinsky-5.svg", diagram 5)
           ]

diagram :: Int -> Element
diagram n = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) (2^(n-1)+2) (2^(n-1)+2)
                >>= addChildren (map symbol [1..n] ++ [fromRight $ Right Default.use >>= Use.href ("#symbol" ++ show n)])



triangle :: Element
triangle = fromRight $ Right Default.path
  >>= Path.strokewidth 0.05
  >>= Path.stroke "black"
  >>= Path.fill "none"
  >>= Path.d [M False 0 0, L False 0.5 (sqrt 3/2),L False 1 0, Z False]


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
       , fromRight $ Right Default.use >>= Use.x (fromIntegral l / 2) >>= Use.y 0 >>= Use.href ("#symbol" ++ show (n-1))
       , fromRight $ Right Default.use >>= Use.x (fromIntegral l / 4) >>= Use.y (sqrt 3/4*fromIntegral l) >>= Use.href ("#symbol" ++ show (n-1))
    ]
    where l = 2^(n-1) :: Int


