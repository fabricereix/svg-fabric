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
