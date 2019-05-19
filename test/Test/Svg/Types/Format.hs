{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Svg.Types.Format where

import Test.Framework
import Svg.Types.Format
import Svg.Types.Core


test_formatLength = do
  assertEqual "1"        $ formatLength (Length 1)
  assertEqual "1.1"      $ formatLength (Length 1.1)
  assertEqual "0.333333" $ formatLength (Length (1/3))
  assertEqual "0.666667" $ formatLength (Length (2/3))
  assertEqual "-1"       $ formatLength (Length (-1))
  assertEqual "-1.1"     $ formatLength (Length (-1.1))


test_formatSegment = do
  assertEqual "M10,10"   $ formatCommand (M False 10 10)
  assertEqual "m1.1,2"   $ formatCommand (M True 1.10 2)
  assertEqual "L0,-1"    $ formatCommand (L False 0 (-1))


test_formatPath = do
  assertEqual "M10,10"   $ formatPath $ Path [M False 10 10]
  assertEqual "m1.1,2"   $ formatPath $ Path [M True 1.10 2]


test_formatTransform = do
  assertEqual "matrix(3 1 -1 3 30 40)"  $ format $ Matrix 3 1 (-1) 3 30 40
  assertEqual "translate(50)"           $ format $ Translate 50 0
  assertEqual "translate(0 50)"         $ format $ Translate 0 50
  assertEqual "scale(4)"                $ format $ Scale 4 4
  assertEqual "scale(1 4)"              $ format $ Scale 1 4
  assertEqual "rotate(100)"             $ format $ Rotate 100 0 0
  assertEqual "rotate(100 10 10)"       $ format $ Rotate 100 10 10
  assertEqual "skewX(30)"               $ format $ SkewX 30
  assertEqual "skewY(30)"               $ format $ SkewY 30
    where format = formatBasicTransform



