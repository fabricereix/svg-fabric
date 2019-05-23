module Svg.Types.Format where

import Svg.Types.Core
--import Data.List


formatLength :: Length -> String
formatLength (Length x) = formatDouble 6 x

formatPercentage :: Percentage -> String
formatPercentage (Percentage x) = show x

formatPaint :: Paint -> String
formatPaint (Color s) = s

formatRemoveFreeze :: RemoveFreeze -> String
formatRemoveFreeze REMOVE = "remove"
formatRemoveFreeze FREEZE = "freeze"

formatRemovefreeze :: RemoveFreeze -> String
formatRemovefreeze REMOVE = "remove"
formatRemovefreeze FREEZE = "freeze"

formatDouble :: Int -> Double -> String
formatDouble n x = sign ++ int ++ dec
   where (integral,frac) = properFraction x :: (Int,Double)
         decimal = show ((round $ fromIntegral((10::Int)^n) * abs frac) :: Int)
         int = show (abs integral)
         sign = if signum x < 0 then "-" else ""
         dec = if decimal == "0" then "" else "." ++ stripSuffix '0' (replicate (n-length decimal) '0' ++ decimal)

stripSuffix :: Char -> String -> String
stripSuffix _ [] = []
stripSuffix c xs = if last xs == c then stripSuffix c (init xs) else xs


formatViewbox :: Viewbox -> String
formatViewbox (Viewbox xmin ymin w h) = unwords $ map (formatDouble 6) [xmin, ymin, w, h]

formatNumber :: Number -> String
formatNumber = undefined

formatAuto :: Auto -> String
formatAuto AUTO = "auto"

formatPoints :: Points -> String
formatPoints (Points points) = unwords $ map formatPoint points

formatPoint :: (Double,Double) -> String
formatPoint (x,y) = formatDouble 3 x ++ "," ++ formatDouble 3 y

formatId :: Id -> String
formatId (Id s) = s

formatPath :: Path -> String
formatPath (Path cs) = unwords $ map formatCommand cs

formatClasses :: Classes -> String
formatClasses (Classes cs) = unwords cs

formatTransform :: Transform -> String
formatTransform (Transform ts) = unwords $ map formatBasicTransform ts

formatBasicTransform :: BasicTransform -> String
formatBasicTransform (Matrix a b c d e f) = "matrix("
                                          ++ formatDouble 6 a ++ " "
                                          ++ formatDouble 6 b ++ " "
                                          ++ formatDouble 6 c ++ " "
                                          ++ formatDouble 6 d ++ " "
                                          ++ formatDouble 6 e ++ " "
                                          ++ formatDouble 6 f ++ ")"
formatBasicTransform (Translate x 0) = "translate(" ++ formatDouble 6 x ++ ")"
formatBasicTransform (Translate x y) = "translate("
                                     ++ formatDouble 6 x ++ " "
                                     ++ formatDouble 6 y ++ ")"
formatBasicTransform (Scale x y) =  "scale("
                                 ++ formatDouble 6 x
                                 ++ (if x == y then "" else " " ++ formatDouble 6 y)
                                 ++ ")"
formatBasicTransform (Rotate a x y)  = "rotate("
    ++ formatDouble 6 a
    ++ (if x == 0 && y == 0 then "" else " " ++ formatDouble 6 x ++ " " ++ formatDouble 6 y)
    ++ ")"
formatBasicTransform (SkewX a) = "skewX(" ++ formatDouble 6 a ++ ")"
formatBasicTransform (SkewY a) = "skewY(" ++ formatDouble 6 a ++ ")"





formatCommand :: Command -> String
formatCommand (M relative x y) = let command = if relative then "m" else "M"
                                 in command ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (L relative x y) = let command = if relative then "l" else "L"
                                 in command ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (H relative x) = let command = if relative then "h" else "H"
                               in command ++ formatDouble 6 x
formatCommand (V relative y) = let command = if relative then "v" else "V"
                               in command ++ formatDouble 6 y
formatCommand (Z relative) = if relative then "z" else "Z"
formatCommand (C relative x1 y1 x2 y2 x y) = let command = if relative then "c" else "C"
                               in command ++ formatDouble 6 x1 ++ "," ++ formatDouble 6 y1
                                   ++ " " ++ formatDouble 6 x2 ++ "," ++ formatDouble 6 y2
                                   ++ " " ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (S relative x2 y2 x y) = let command = if relative then "s" else "S"
                                       in command ++ formatDouble 6 x2 ++ "," ++ formatDouble 6 y2
                                           ++ " " ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (Q relative x1 y1 x y) = let command = if relative then "q" else "Q"
                                       in command ++ formatDouble 6 x1 ++ "," ++ formatDouble 6 y1
                                           ++ " " ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (T relative x y) = let command = if relative then "s" else "T"
                                 in command ++ " " ++ formatDouble 6 x ++ "," ++ formatDouble 6 y
formatCommand (A relative rx ry xAxisRotation largeArcFlag sweepFlag x y) =
                                 let command = if relative then "a" else "A"
                                 in command ++ formatDouble 6 rx ++ "," ++ formatDouble 6 ry
                                         ++ " " ++ formatDouble 6 xAxisRotation
                                         ++ "," ++ formatDouble 6 largeArcFlag
                                         ++ "," ++ formatDouble 6 sweepFlag
                                         ++ " " ++ formatDouble 6 x ++ "," ++ formatDouble 6 y


formatContenttype :: ContentType -> String
formatContenttype (ContentType s) = s

