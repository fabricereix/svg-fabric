{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Svg.Types.Parser where

import Test.Framework
import Svg.Types.Parser
import Svg.Types.Core
import Prelude hiding(id)


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

  assertEqual (Right (Path [M False 10 10, C False 20 20 40 20 50 10]))    $ path "M10 10 C 20 20, 40 20, 50 10"

  print $ path "M40.000000,87.000000 L194.046555,87.000000 C194.051235,68.993509,194.042435,54.117931,194.000665,54.000622 L245.499993,102.000000 L193.999993,150.000000 C193.999993,150.000000,194.015443,136.165430,194.028753,119.000000 L39.999990,119.000000 Z M194.000000,54.000000 C194.000220,53.999790,194.000440,54.000000,194.000670,54.000620 Z "

  --  print $ path "M40.000000,87.000000 L194.046555,87.000000 C194.051235,68.993509,194.042435,54.117931,194.000665,54.000622 L245.499993,102.000000 L193.999993,150.000000 C193.999993,150.000000,194.015443,136.165430,194.028753,119.000000 L39.999990,119.000000 Z M194.000000,54.000000 "

  --print $ path " Z "


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

test_points = do
  assertEqual (Right (Points []))            $ points ""
  assertEqual (Right (Points [(0,0),(1,1)])) $ points "0,0 1,1"

test_id = do
  assertEqual (Right (Id "id1"))  $ id "id1"
  assertEqual (Right (Id "id1"))  $ id "id1 "

