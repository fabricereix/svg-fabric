{-# LANGUAGE OverloadedStrings #-}
module Svg.Gallery.Sunflower where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg      as Svg
import           Text.XML
import qualified Svg.Setter.Circle as Circle
import           Svg.Setter
import           Svg.Gallery.Helper


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = map (\n-> ("sunflower-" ++ show n ++ ".svg", diagram n)) [10,100,1000]


diagram  :: Int -> Element
diagram n = roundCircle 1 $ fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-size) (-size) (2*size) (2*size)
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.05
            >>= addChildren (
                  fromRight (Right Default.style >>= addText "circle {r:0.6}"):
                  map floret coords)
     where size = 1 + fromIntegral (ceiling (sqrt $ fromIntegral n :: Double) :: Int)
           coords = map (\i-> (sqrt (fromIntegral i), fromIntegral i*goldenAngle)) [1..n]


goldenAngle :: Double
goldenAngle = pi * (3 - sqrt 5) -- 2.4


floret :: (Double, Double) -> Element
floret (r,theta) = fromRight $ Right Default.circle
                     >>= Circle.cx x
                     >>= Circle.cy y
                     -- >>= Circle.r 0.6
    where (x,y) = fromPolar (r,theta)


