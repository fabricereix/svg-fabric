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
             ("sierpinsky-5.svg", diagram)
           ]

diagram :: Element
diagram = fromRight $ Right Default.svg
                >>= Svg.width 500
                >>= Svg.height 500
                >>= Svg.viewBox (-1) (-1) 8 8
                >>= addChildren [
                      symbol1
                    , symbol2
                    , use
                    ]

use :: Element
use = fromRight $ Right Default.use
  >>= Use.x 0
  >>= Use.y 0
  >>= Use.href "#symbol2"


triangle :: Element
triangle = fromRight $ Right Default.path
  >>= Path.strokewidth 0.1
  >>= Path.stroke "black"
  >>= Path.fill "black"
  >>= Path.d [M False 0 0, L False 0.5 (sqrt 3/2),L False 1 0, Z False]


symbol1 :: Element
symbol1 = fromRight $ Right Default.symbol
               >>= Symbol.id "symbol1"
               >>= Symbol.width 1
               >>= Symbol.height 1
               >>= addChildren [
                      triangle
                   ]

symbol2 :: Element
symbol2 = fromRight $ Right Default.symbol
               >>= Symbol.id "symbol2"
               >>= Symbol.width 2
               >>= Symbol.height 2
               >>= addChildren [
                     fromRight $ Right Default.use >>= Use.x 0 >>= Use.y 0 >>= Use.href "#symbol1"
                   , fromRight $ Right Default.use >>= Use.x 1 >>= Use.y 0 >>= Use.href "#symbol1"
                   , fromRight $ Right Default.use >>= Use.x 0.5 >>= Use.y (sqrt 3/2) >>= Use.href "#symbol1"
                   ]

symbol3 :: Element
symbol3 = fromRight $ Right Default.symbol
               >>= Symbol.id "symbol3"
               >>= Symbol.width 4
               >>= Symbol.height 4
               >>= addChildren [
                     fromRight $ Right Default.use >>= Use.x 0 >>= Use.y 0 >>= Use.href "#symbol2"
                   , fromRight $ Right Default.use >>= Use.x 2 >>= Use.y 0 >>= Use.href "#symbol2"
                   , fromRight $ Right Default.use >>= Use.x 1 >>= Use.y (sqrt 3) >>= Use.href "#symbol2"
                   ]

