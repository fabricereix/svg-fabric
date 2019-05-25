{-# LANGUAGE OverloadedStrings     #-}
module Svg.Gallery.Palette where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg      as Svg
import           Text.XML
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Text as Text
import qualified Svg.Setter.Path as Path
-- import qualified Svg.Setter.G as G
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
rectangularPalette y (name,colors) = fromRight $ Right Default.g
           >>= addChildren (label:palette)
    where label = fromRight (Right Default.text >>= Text.y (y+2) >>= Text.class' ["label"] >>= addText name)
          palette =map (\(i, color)->
                      fromRight (Right Default.rect
                         >>= Rect.x (6+fromIntegral i)
                         >>= Rect.y y
                         >>= Rect.width 1
                         >>= Rect.height 5
                         >>= Rect.fill color
                  )) $ zip [0::Int ..] colors

type Color = String
brewerOranges, brewerBlues :: [Color]
brewerOranges = ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]
brewerBlues =  ["#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"]



colorwheel :: Element
colorwheel = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-2) (-2) 4 4
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.05
            >>= addChildren (arcs 5)

circularPoints :: Int -> [(Double,Double)]
circularPoints n = map (fromPolar . (\i->(1, 2*pi*fromIntegral i/fromIntegral n))) [0..n]

--arcs :: Int -> [(Point,(Double,Double))]
--arcs n = map (\((x1,y1),(x2,y2))->((x1,y1),(x2-x1,y2-y1))) $ zip (circularPoints n) (tail $ circularPoints n)


arcs :: Int -> [Element]
arcs n = map (\((x1,y1),(x2,y2))->arc (x1,y1) (x2-x1,y2-y1)) $ zip (tail $ circularPoints n) (circularPoints n)


arc :: (Double,Double) -> (Double,Double) -> Element
arc (x,y) (dx,dy) = fromRight $ Right Default.path
   >>= Path.fill "yellow"
   >>= Path.d [ M False 0 0, L False x y, A True 1 1 0 0 0 dx dy]




