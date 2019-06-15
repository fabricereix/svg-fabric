{-# LANGUAGE OverloadedStrings #-}
module Svg.Gallery.Bezier where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Path     as Path
import qualified Svg.Setter.Svg      as Svg
import qualified Svg.Setter.Text     as Text
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
diagrams = [("bezier-flatness.svg", diagram)]


diagram :: Element
diagram = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-0.5) (-0.5) 3 2
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.01
            >>= addChildren [
                  fromRight $ Right Default.style >>= addText style
                , path bezier1 (head d3_10)
                , label (1.5, 0) (head d3_10) (cs $ show $ flatness bezier1)
                , path bezier2 (d3_10 !! 1)
                , label (1.5, 0.2) (d3_10 !! 1) (cs $ show $ flatness bezier2)
                ]
          where bezier1 = ((0,0), (0,1),(1,1),(1,0))
                bezier2 = ((0,0), (0,0.5),(1,0.5),(1,0))

style :: T.Text
style = ".label {font-size: 0.1px}"

type Color = String

d3_10 :: [Color]
d3_10 = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]



type CubicBezier = ((Double, Double), (Double, Double), (Double, Double), (Double, Double))



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




