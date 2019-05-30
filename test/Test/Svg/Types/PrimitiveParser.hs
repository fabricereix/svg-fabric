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

--runParserWithLeftOver :: Parser a -> b -> String -> (a,String)
--runParserWithLeftOver p state s = case runParser p state "" s of
--    Left e -> error "expect failure"
--    Right x -> do leftover <- manyTill anyToken eof
--                  (x,leftover)

expectError p s = case parseWithLeftOver p s of
   Left x  -> show x
   Right x -> "parsing success " ++ show x


test_command_parser = do
  assertEqual (Right (M False 0 0))   $ runParser command' Nothing "" "M0,0"
  assertEqual (Right (L False 1 2))   $ runParser command' Nothing "" "L1,2"
  assertEqual (Right (L False 1 2))   $ runParser command' Nothing "" "L 1,2"
  assertEqual (Right (H True 1))      $ runParser command' Nothing "" "h1"
  assertEqual (Right (H True 1))      $ runParser command' (Just 'h') "" "1"



test_transform_parser = do
  assertEqual (Right (Translate 1 1,"")) $ parseWithLeftOver basicTransform "translate(1 1)"
  assertEqual (Right (Translate 1 1,"")) $ parseWithLeftOver basicTransform "translate(1 1)"
  assertEqual (Right (Rotate 30 (1,1),"")) $ parseWithLeftOver basicTransform "rotate(30 1 1)"
  assertEqual (Right (Rotate 30 (0,0),"")) $ parseWithLeftOver basicTransform "rotate(30)"

test_class = do
  assertEqual (Right ("class1", "")) $ parseWithLeftOver classParser "class1"
  assertEqual (Right ("class1", "")) $ parseWithLeftOver classParser "class1 "

test_point = do
  assertEqual (Right ((0,0), ""))    $ parseWithLeftOver point "0,0"
  assertEqual (Right ((0,0), ""))    $ parseWithLeftOver point "0,0 "
  assertEqual (Right ((0,0), "1,1")) $ parseWithLeftOver point "0,0 1,1"
  assertEqual (Right ((0,0), "1,1")) $ parseWithLeftOver point "0,0,1,1"
  assertEqual (Right ((0,0), ""))    $ parseWithLeftOver point "0 0"


test_double = do
  assertEqual (Right (0,"")) $ parseWithLeftOver double "0"
  assertEqual (Right (0," ")) $ parseWithLeftOver double "0 "
  assertEqual (Right (0,"x")) $ parseWithLeftOver double "0x"
  assertEqual "(line 1, column 1):\nunexpected end of input\nexpecting double" $ expectError double ""
  assertEqual "(line 1, column 1):\nunexpected \" \"\nexpecting double" $ expectError double " 0"

