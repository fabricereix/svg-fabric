{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Svg.Types.Format
import {-@ HTF_TESTS @-} Test.Svg.Xml
import {-@ HTF_TESTS @-} Test.Svg.Combinator
import {-@ HTF_TESTS @-} Test.Svg.Getter
import {-@ HTF_TESTS @-} Test.Svg.Attribute

main = htfMainWithArgs ["-q"] htf_importedTests
