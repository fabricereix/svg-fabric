{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Svg.Types.Parser where

import Test.Framework
import Svg.Types.Parser
import Svg.Types.Core

import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.String

-- -- test utils
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> manyTill anyToken eof) ""

expectError p s = case parseWithLeftOver p s of
   Left x  -> show x
   Right x -> "parsing success " ++ show x



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


test_command_parser = do
  assertEqual (Right (M False 0 0,"")) $ parseWithLeftOver command' "M0,0"
  assertEqual (Right (L False 1 2,"")) $ parseWithLeftOver command' "L1 2"
  assertEqual (Right (H True 2,"")) $ parseWithLeftOver command' "h 2"
  assertEqual (Right (V True 1,"v2")) $ parseWithLeftOver command' "l1v2"


