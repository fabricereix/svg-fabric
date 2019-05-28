{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Svg.Types.Parser where

import Test.Framework
import Svg.Types.Parser
import Svg.Types.Core


test_removeFreeze = do
  assertEqual (Right REMOVE)                               $ removeFreeze "remove"
  assertEqual (Right FREEZE)                               $ removeFreeze "freeze"
  assertEqual (Left "Can not parse \"x\" to RemoveFreeze") $ removeFreeze "x"


test_auto = do
  assertEqual (Right AUTO)                         $ auto "auto"
  assertEqual (Left "Can not parse \"x\" to auto") $ auto "x"


test_viewbox = do
  assertEqual (Right (Viewbox 0 0 5 4)) $ viewbox "0 0 5 4"
  assertEqual (Right (Viewbox 0.1 0.2 0.3 0.4)) $ viewbox "0.1 0.2 0.3 0.4"
  assertEqual (Left "Can not parse \"x\" to viewport") $ viewbox "x"


xtestPath = do
  assertEqual (Right (Path [])) $ path ""
  assertEqual (Right (Path [])) $ path ""



