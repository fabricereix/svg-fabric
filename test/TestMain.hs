{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Svg.Types.Format
import {-@ HTF_TESTS @-} Test.Svg.Types.Parser
-- import {-@ HTF_TESTS @-} Test.Svg.Xml
import {-@ HTF_TESTS @-} Test.Svg.Combinator
import {-@ HTF_TESTS @-} Test.Svg.Setter
--import {-@ HTF_TESTS @-} Test.Svg.Getter
--import {-@ HTF_TESTS @-} Test.Svg.Attribute
import {-@ HTF_TESTS @-} Test.Svg.Validator

main = htfMainWithArgs ["-q"] htf_importedTests
