{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Svg.Core.Value

main = htfMainWithArgs ["-q"] htf_importedTests
