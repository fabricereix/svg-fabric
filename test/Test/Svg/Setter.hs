{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Setter where

import Test.Framework
-- import Svg.Elements
-- import Svg.Combinator.Rect
-- import Svg.Types.Core
import           Data.String.Conversions
import           Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Circle as Circle
import qualified Test.Svg.Sample as Sample

-- default prevent explicit failure
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x



test_1 = do
  putStrLn "HELLO"
  putStrLn "HELLO"
  print Default.rect
  printXMLElement Default.rect
  --assertEqual (Left "circle element - should be a rect element")              $ Right Default.circle >>= Rect.x "1"
  print $ Right Default.rect >>= Rect.xLength 1
  print $ Right Default.rect >>= Circle.fill "black"
  assertEqual (Left "should be a circle instead of rect")
              (Right Default.rect >>= Circle.fill "black")
  assertEqual (Left "Attribute fill already set")
              (Right Default.rect >>= Rect.fill "black" >>= Rect.fill "red")

  assertEqual Sample.w3c $ fromRight $
    Right Default.svg
      >>= Svg.width 100
      >>= Svg.height 100


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



