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
