{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Combinator where

import Test.Framework
#import Data.String.Conversions
import Svg.Elements
import Svg.Combinator.Rect
import Svg.Types.Core


test_1 = do
  putStrLn "HELLO"
  assertEqual "1" ("1"::String)
  print $ defaultRect
  assertEqual (Right (Rect [] (OneOf2 (Length 1.0)) (OneOf2 (Length 2.0)) (OneOf3 AUTO) (OneOf3 AUTO) Nothing))
              (Right defaultRect >>= x "1" >>= y "2")
  assertEqual (Left "Can not parse value \"A\" for attribute x")
              ((Right defaultRect) >>= x "A")





