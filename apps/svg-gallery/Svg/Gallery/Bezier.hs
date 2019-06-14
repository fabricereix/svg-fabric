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
                , path bezier1
                , label (1.5, 0) (cs $ show $ flatness bezier1)
                ]
          where bezier1 = ((0,0), (0,1),(1,1),(1,0))

style :: T.Text
style = ".label {font-size: 0.2px}"


type CubicBezier = ((Double, Double), (Double, Double), (Double, Double), (Double, Double))



label :: (Double,Double) -> T.Text -> Element
label (x,y) t = fromRight $ Right Default.text
   >>= Text.class' ["label"]
   >>= Text.x x
   >>= Text.y y
   >>= addText t


flatness :: CubicBezier -> Double
flatness _ = 1

path :: CubicBezier -> Element
path ((xa,ya), (x1,y1),(x2,y2), (xb,yb))= fromRight $ Right Default.path
            -- >>= Path.strokewidth 0.1
            >>= Path.stroke "black"
            >>= Path.fill "none"
            >>= Path.d (M False xa ya:[C False x1 y1 x2 y2 xb yb])




