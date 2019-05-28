{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Svg.Types.PrimitiveParser where

import Test.Framework
import Svg.Types.PrimitiveParser
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


test_command_parser = do
  assertEqual (Right (M False 0 0,"")) $ parseWithLeftOver command' "M0,0"
  assertEqual (Right (M False 0 0,"")) $ parseWithLeftOver command' "M0 0"
  assertEqual (Right (L False 1 2,"")) $ parseWithLeftOver command' "L1 2"
  assertEqual (Right (H True 2,"")) $ parseWithLeftOver command' "h 2"
  assertEqual (Right (H True 1,"v2")) $ parseWithLeftOver command' "h1v2"
  assertEqual (Right (H True 1,"")) $ parseWithLeftOver command' "h 1"
  assertEqual (Right (Z True,"")) $ parseWithLeftOver command' "z"


test_double = do
  assertEqual (Right (0,"")) $ parseWithLeftOver double "0"
  assertEqual (Right (0," ")) $ parseWithLeftOver double "0 "
  assertEqual (Right (0,"x")) $ parseWithLeftOver double "0x"
  assertEqual "(line 1, column 1):\nunexpected end of input\nexpecting double" $ expectError double ""
  assertEqual "(line 1, column 1):\nunexpected \" \"\nexpecting double" $ expectError double " 0"

