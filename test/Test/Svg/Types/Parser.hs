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


test_path = do
  assertEqual (Right (Path []))                          $ path ""
  assertEqual (Right (Path [M True 1 1]))                $ path "m1,1"
  assertEqual (Right (Path [L True 1 2, L True 3 4]))    $ path "l1 2 3 4"


test_transform = do
  assertEqual (Right (Transform []))                     $ transform ""
  assertEqual (Right (Transform [Translate 1 2]))        $ transform "translate(1 2)"
  assertEqual (Right (Transform [Translate 1 2]))        $ transform "translate( 1 2 )"
  assertEqual (Right (Transform [Translate 1.1 0]))      $ transform "translate(1.1)"
  assertEqual (Right (Transform [Translate 1.1 0]))      $ transform "translate(1.1) "
  assertEqual (Left "(line 1, column 14):\nunexpected 'x'\nexpecting \"matrix\", \"translate\", \"scale\", \"rotate\", \"skewx\", \"skewy\", white space or end of input")
                                                         $ transform "translate(1) x"
  assertEqual (Right (Transform [Scale 1 2]))           $ transform "scale(1 2)"
  assertEqual (Right (Transform [Scale 1 1]))           $ transform "scale(1)"
  assertEqual (Right (Transform [Matrix 1 2 3 4 5 6]))   $ transform "matrix(1 2 3 4 5 6)"

  assertEqual (Left "(line 1, column 1):\nunexpected 'u'\nexpecting \"matrix\", \"translate\", \"scale\", \"rotate\", \"skewx\", \"skewy\", white space or end of input")  $ transform "undefined(0 0)"
  assertEqual (Left "(line 1, column 16):\nunexpected 'u'\nexpecting \"matrix\", \"translate\", \"scale\", \"rotate\", \"skewx\", \"skewy\", white space or end of input")  $ transform "translate(1 1) undefined(0 0)"

test_classes = do
  assertEqual (Right (Classes [])) $ classes ""
  assertEqual (Right (Classes ["class1", "class2"])) $ classes "class1 class2"

