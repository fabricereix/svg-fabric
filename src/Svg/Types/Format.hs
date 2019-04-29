module Svg.Types.Format where

import Svg.Types.Core


formatLength :: Length -> String
formatLength (Length x) = formatDouble 6 x

formatPercentage :: Percentage -> String
formatPercentage (Percentage x) = show x

formatPaint :: Paint -> String
formatPaint (Color s) = s

formatRemoveFreeze :: RemoveFreeze -> String
formatRemoveFreeze REMOVE = "remove"
formatRemoveFreeze FREEZE = "freeze"

formatDouble :: Int -> Double -> String
formatDouble n x = show integer ++ if decimal == "0" then "" else "." ++ (stripSuffix '0' $ replicate (n-length decimal) '0' ++ decimal)
   where (integer,dec) = properFraction x :: (Int,Double)
         decimal = show ((round $ fromIntegral((10::Int)^n) * dec) :: Int)

stripSuffix :: Char -> String -> String
stripSuffix _ [] = []
stripSuffix c xs = if last xs == c then stripSuffix c (init xs) else xs


formatViewport :: Viewport -> String
formatViewport = undefined

formatNumber :: Number -> String
formatNumber = undefined

formatAuto :: Auto -> String
formatAuto AUTO = "auto"

