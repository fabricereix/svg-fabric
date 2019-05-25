{-# LANGUAGE OverloadedStrings     #-}
module Svg.Gallery.Palette where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg      as Svg
import           Text.XML
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Text as Text
import qualified Svg.Setter.Path as Path
import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import           Svg.Gallery.Helper
import qualified Data.Text as T
import Svg.Types.Core


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [
    ("palette-brewerset.svg", brewerset)
  , ("colorwheel.svg", colorwheel)
  ]



brewerset  :: Element
brewerset = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox 0 0 30 30
            >>= Svg.stroke "none"
            >>= Svg.strokewidth 0.05
            >>= addChildren [
                  style
                , rectangularPalette 0 ("Oranges",brewerOranges)
                , rectangularPalette 6 ("Blues",brewerBlues)
                ]

style :: Element
style = fromRight $ Right Default.style
   >>= addText ".label {font-size: 1px}"

rectangularPalette :: Double -> (T.Text, [Color]) -> Element
rectangularPalette y (name,cs) = fromRight $ Right Default.g
           >>= addChildren (label:palette)
    where label = fromRight (Right Default.text >>= Text.y (y+2) >>= Text.class' ["label"] >>= addText name)
          palette =map (\(i, color)->
                      fromRight (Right Default.rect
                         >>= Rect.x (6+fromIntegral i)
                         >>= Rect.y y
                         >>= Rect.width 1
                         >>= Rect.height 5
                         >>= Rect.fill color
                  )) $ zip [0::Int ..] cs

type Color = String
brewerOranges, brewerBlues :: [Color]
brewerOranges = ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]
brewerBlues =  ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"]



colorwheel :: Element
colorwheel = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 1000
            >>= Svg.viewBox (-2) (-2) 4 8
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.05
            >>= addChildren [
                  addTransform [Translate 0.5 0] $ wheel colors
                , translate (0,3) (wheel colors)
                ]

-- Helper
translate :: (Double, Double) -> Element -> Element
translate (x,y) element = fromRight $ Right element >>= G.transform [Translate     x y]


colors :: [Color]
colors = ["red", "blue", "green"]


wheel :: [Color] -> Element
wheel cs = fromRight $ Right Default.g
             >>= addChildren (map (slice (length cs)) $ zip [1..] cs)


slice :: Int -> (Int, Color) -> Element
slice n (i, color) = fromRight $ Right Default.path
   >>= Path.fill color
   >>= Path.d [ M False 0 0, L False x y, A False 1 1 0 0 0 dx dy, Z False]
   where (x,y) = fromPolar (1, 2*pi*fromIntegral i/fromIntegral n)
         (dx,dy) = fromPolar (1, 2*pi*fromIntegral (i-1)/fromIntegral n)






