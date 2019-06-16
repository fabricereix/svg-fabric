{-# LANGUAGE OverloadedStrings #-}
module Svg.Gallery.Bezier where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Path     as Path
import qualified Svg.Setter.Svg      as Svg
import qualified Svg.Setter.Text     as Text
import qualified Svg.Setter.Circle   as Circle
import           Text.XML
-- import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import           Svg.Gallery.Helper
import Svg.Types.Core
import qualified Data.Text as T
import Data.String.Conversions


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [
    ("bezier-flatness.svg", diagram)
  , ("bezier-interpolation.svg", diagramInterpolation)
  ]

diagramInterpolation :: Element
diagramInterpolation = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-0.5) (-0.5) 3 2
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.01
            >>= addChildren ([
                  fromRight ( Right Default.style >>= addText style)
                , label (0,-0.2) "black" "Interpolation"
                , path bezier "black"
                ] ++ map circle points)
          where bezier = ((0,0), (0.5,1),(1,1), (1,0))
                points = map (polynomial bezier) [0, 1/10..1]




type ControlPoint = (Double, Double)
type Polynomial = (Double, Double, Double, Double)

bezier1 :: (ControlPoint, ControlPoint, ControlPoint, ControlPoint)
bezier1 = ((0,0), (0.5,1),(1,1), (1,0))

-- polynomial1 :: (Polynomial, Polynomial)
-- polynomial1 = polynomial bezier1

polynomial :: CubicBezier -> Double -> (Double,Double)
polynomial ((x0,y0), (x1,y1), (x2,y2), (x3,y3)) t = (
    sum $ zipWith (*) [1, t, t*t, t*t*t] (polynomialCoefficients (x0,x1,x2,x3))
  , sum $ zipWith (*) [1, t, t*t, t*t*t] (polynomialCoefficients (y0,y1,y2,y3))
  )


--points :: Int -> [Double]
--points n = [0,(1/fromIntegral n)..1]


polynomialCoefficients :: (Double,Double,Double,Double) -> [Double]
polynomialCoefficients (x0,x1,x2,x3) = [
    x0
  , -3*x0 + 3*x1
  , 3*x0-6*x1+3*x2
  , -x0 + 3*x1 -3*x2 + x3
  ]



diagram :: Element
diagram = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-0.5) (-0.5) 3 2
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.01
            >>= addChildren ([
                  fromRight ( Right Default.style >>= addText style)
                , label (0,-0.2) "black" "Flatness"
                ]
                ++ zipWith path beziers d3_10
                ++ map (\(f, c, xy)->label xy c (cs $ show f)) (zip3
                    (map (roundDouble 3 . flatness) beziers)
                    d3_10
                    (map (\i->(1.5,0.2*fromIntegral (i::Int))) [0..])
                   ))
          where beziers = [
                    ((0,0), (0.2,0.01), (0.8,0.01), (1,0))
                  , ((0,0), (0.2,0.05), (0.8,0.05), (1,0))
                  , ((0,0), (0.2,0.1),  (0.8,0.1),  (1,0))
                  , ((0,0), (0.2,0.2),  (0.8,0.2),  (1,0))
                  , ((0,0), (0.2,0.5),  (0.8,0.5),  (1,0))
                  , ((0,0), (0.2,1),    (0.8,1),    (1,0))
                  ]

style :: T.Text
style = ".label {font-size: 0.1px}"

type Color = String

d3_10 :: [Color]
d3_10 = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]



type CubicBezier = (ControlPoint, ControlPoint, ControlPoint, ControlPoint)


circle :: (Double,Double) -> Element
circle (x,y) = fromRight $ Right Default.circle
   >>= Circle.cx x
   >>= Circle.cy y
   >>= Circle.r 0.02


label :: (Double,Double) -> Color -> T.Text -> Element
label (x,y) color t = fromRight $ Right Default.text
   >>= Text.class' ["label"]
   >>= Text.x x
   >>= Text.y y
   >>= Text.fill color
   >>= Text.stroke color
   >>= addText t


flatness :: CubicBezier -> Double
flatness ((b0_x, b0_y), (b1_x,b1_y),(b2_x,b2_y),(b3_x,b3_y)) =
   let ux = 3.0 * b1_x - 2.0 * b0_x - b3_x
       uy = 3.0 * b1_y - 2.0 * b0_y - b3_y
       vx = 3.0 * b2_x - 2.0 * b3_x - b0_x
       vy = 3.0 * b2_y - 2.0 * b3_y - b0_y
   in max (ux*ux) (vx*vx)  + max (uy*uy) (vy*vy)


path :: CubicBezier -> Color -> Element
path ((xa,ya), (x1,y1),(x2,y2), (xb,yb)) color = fromRight $ Right Default.path
            -- >>= Path.strokewidth 0.1
            >>= Path.stroke color
            >>= Path.fill "none"
            >>= Path.d (M False xa ya:[C False x1 y1 x2 y2 xb yb])




