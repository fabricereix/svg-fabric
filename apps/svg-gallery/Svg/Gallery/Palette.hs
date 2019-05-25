{-# LANGUAGE OverloadedStrings #-}
module Svg.Gallery.Palette where
import qualified Svg.DefaultElements as Default
import           Svg.Setter
import qualified Svg.Setter.Path     as Path
import qualified Svg.Setter.Rect     as Rect
import qualified Svg.Setter.Svg      as Svg
import qualified Svg.Setter.Text     as Text
import           Text.XML
-- import           Svg.Types.Core
import qualified Data.Text           as T
import           Svg.Gallery.Helper
import           Svg.Types.Core


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [
    ("palettes.svg", palettes)
  ]



palettes  :: Element
palettes = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox 0 0 30 30
            >>= Svg.stroke "none"
            >>= Svg.strokewidth 0.05
            >>= addChildren [
                  style
                , label (1,1) "Brewer Oranges"
                , addTransform [Translate 0.5 0.5] $ rectangularPalette brewerOranges
                , label (11,1) "Brewer Blues"
                , addTransform [Translate 11 0.5] $ rectangularPalette brewerBlues
                , label (1,9) "RYB Color Wheel"
                , addTransform [Translate 5 14, Scale 4 4] $ wheel rybColors
                , label (11,9) "10 constrasting color from d3"
                , addTransform [Translate 15 14, Scale 4 4] $ wheel d3_10
                ]

style :: Element
style = fromRight $ Right Default.style
   >>= addText ".label {font-size: 1px}"


label :: (Double,Double) -> T.Text -> Element
label (x,y) t = fromRight $ Right Default.text
   >>= Text.class' ["label"]
   >>= Text.x x
   >>= Text.y y
   >>= addText t


rectangularPalette :: [Color] -> Element
rectangularPalette cs = fromRight $ Right Default.g
           >>= addChildren slices
    --where label = fromRight (Right Default.text >>= Text.y 0.5 >>= Text.class' ["label"] >>= addText name)
    where slices = map (\(i, color)->
                      fromRight (Right Default.rect
                         >>= Rect.x (fromIntegral i)
                         >>= Rect.y 1
                         >>= Rect.width 1
                         >>= Rect.height 5
                         >>= Rect.fill color
                  )) $ zip [0::Int ..] cs

type Color = String
brewerOranges, brewerBlues :: [Color]
brewerOranges = ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]
brewerBlues =  ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"]

colors :: [Color]
colors = ["red", "blue", "green"]

rybColors :: [Color]
rybColors = [
    "#ff0000", "#ff4900", "#ff7400", "#ff9200"
  , "#ffaa00", "#ffbf00", "#ffd300", "#ffe800"
  , "#ffff00", "#ccf600", "#9fee00", "#67e300"
  , "#00cc00", "#00af64", "#009999", "#0b61a4"
  , "#1240ab", "#1b1bb3", "#3914af", "#530fad"
  , "#7109aa", "#a600a6", "#cd0074", "#e40045"
  ]

d3_10 :: [Color]
d3_10 = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]

wheel :: [Color] -> Element
wheel cs = fromRight $ Right Default.g
             >>= addChildren slices
   --where label = fromRight $ Right Default.text >>= Text.class' ["label"] >>= addText t
  where  slices = map (slice (length cs)) $ zip [1..] cs


slice :: Int -> (Int, Color) -> Element
slice n (i, color) = fromRight $ Right Default.path
   >>= Path.fill color
   >>= Path.d [ M False 0 0, L False x y, A False 1 1 0 0 0 dx dy, Z False]
   where (x,y) = fromPolar (1, 2*pi*fromIntegral i/fromIntegral n)
         (dx,dy) = fromPolar (1, 2*pi*fromIntegral (i-1)/fromIntegral n)






