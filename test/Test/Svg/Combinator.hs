{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Combinator where

import Test.Framework
-- import Svg.Elements
-- import Svg.Combinator.Rect
-- import Svg.Types.Core
import           Data.String.Conversions
import           Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Combinator.Rect as Rect
import qualified Svg.Combinator.Circle as Circle

-- default prevent explicit failure
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x



test_1 = do
  putStrLn "HELLO"
  putStrLn "HELLO"
  print Default.rect
  printXMLElement Default.rect
  assertEqual (Left "circle element - should be a rect element") $ Right Default.circle >>= Rect.x "1"

  printXMLElement $fromRight $ (Right Default.rect) >>= Rect.x "1"
  printXMLElement $fromRight $ (Right Default.circle) >>= Circle.r "1"
--  printXMLElement $fromRight $ (Right Default.circle) >>= Circle.x "1"
--                                                      >>= Rect.x "x"
  -- assertEqual "1" ("1"::String)
--  print $ defaultRect
  --assertEqual (Right (Rect [] (OneOf2 (Length 1.0)) (OneOf2 (Length 2.0)) (OneOf3 AUTO) (OneOf3 AUTO) Nothing))
 --             (Right defaultRect >>= x "1" >>= y "2")
--  assertEqual (Left "Can not parse value \"A\" for attribute x")
--              ((Right defaultRect) >>= x "A")



printXMLElement :: Element -> IO()
printXMLElement element = putStrLn $ cs $ renderText def
  -- def { rsAttrOrder= const (toOrderedList ["x", "fill"])}
   $ doc element
    where doc root = Document {
                documentPrologue = Prologue {
                    prologueBefore = []
                  , prologueDoctype = Nothing
                  , prologueAfter = []
                  }
              , documentRoot = root
              , documentEpilogue = []
              }



