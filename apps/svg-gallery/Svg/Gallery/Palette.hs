{-# LANGUAGE OverloadedStrings     #-}
module Svg.Gallery.Palette where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg      as Svg
import           Text.XML
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Text as Text
-- import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import           Svg.Gallery.Helper


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [
    ("palette-brewerset.svg", brewerset)
  ]



brewerset  :: Element
brewerset = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox 0 0 10 10
            >>= Svg.stroke "none"
            >>= Svg.strokewidth 0.05
            >>= addChildren [
                  style
                , rectangularPalette
                ]

style :: Element
style = fromRight $ Right Default.style
   >>= addText ".label {font: italic 13px}"

rectangularPalette :: Element
rectangularPalette = fromRight $ Right Default.g
           >>= addChildren (label:palette)
    where label = fromRight (Right Default.text >>= Text.y 20 >>= Text.class' ["label"] >>= addText "Oranges")
          palette =map (\(i, color)->
                      fromRight (Right Default.rect
                         >>= Rect.x (fromIntegral i)
                         >>= Rect.width 1
                         >>= Rect.height 5
                         >>= Rect.fill color
                  )) $ zip [0::Int ..] brewerOranges

type Color = String
brewerOranges :: [Color]
brewerOranges = ["#fff5eb","#fee6ce","#fdd0a2","#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"]



