{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Attribute where

import Test.Framework
#import Data.String.Conversions
import Svg.Elements
import qualified Svg.Attribute.Rect as Rect
import qualified Svg.Attribute.Animate as Animate
--import Svg.Types.Core
--import Svg.Attribute

test_1 = do
  assertEqual "0"            $ Rect.x defaultRect
  assertEqual Nothing        $ Rect.fill defaultRect
--  assertEqual (Just "black") $ Rect.fill $ Rect [] (OneOf2 (Length 0)) (OneOf2 (Length 0)) (OneOf3 AUTO) (OneOf3 AUTO) (Just $ OneOf1 (Color "black"))
  assertEqual "remove"       $ Animate.fill defaultAnimate

--test_2 = do
--  assertEqual []                           $ attributes defaultRect
--  assertEqual [("x","1"),("fill","black")] $ attributes $ Rect [] (OneOf2 (Length 1)) (OneOf2 (Length 0)) (OneOf3 AUTO) (OneOf3 AUTO) (Just $ OneOf1 (Color "black"))



