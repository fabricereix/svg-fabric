{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Getter where

import Test.Framework
#import Data.String.Conversions
import Svg.Elements
import qualified Svg.Getter.Rect as Rect
import Svg.Types.Core


test_1 = do
  putStrLn "HELLO"
  assertEqual "1" ("1"::String)
  assertEqual (OneOf2 (Length 0)) (Rect.x defaultRect)





