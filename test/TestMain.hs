{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Svg.Types.Format
import {-@ HTF_TESTS @-} Test.Svg.Xml

main = htfMainWithArgs ["-q"] htf_importedTests
