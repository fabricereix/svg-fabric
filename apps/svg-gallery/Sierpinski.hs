module Sierpinski where

import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Path as Path
import qualified Svg.Setter.Symbol as Symbol
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
                       symbol
                    ]


symbol :: Element
symbol = fromRight $ Right Default.path
  >>= Path.strokewidth 0.1
  >>= Path.stroke "black"
  >>= Path.fill "black"
  >>= Path.d [M False 0 0, L False 0.5 (sqrt 3/2),L False 1 0, Z False]


triangle1 :: Element
triangle1 = fromRight $ Right Default.symbol
               >>= Symbol.id "triangle1"

