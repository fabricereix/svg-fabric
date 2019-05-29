{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Setter where

import Test.Framework
-- import Svg.Elements
-- import Svg.Combinator.Rect
-- import Svg.Types.Core
import           Data.String.Conversions
import           Text.XML hiding (writeFile)
import qualified Svg.DefaultElements as Default
import           Svg.Setter
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Setter.Circle as Circle
import qualified Svg.Setter.Path as Path
import qualified Svg.Setter.G as G
import qualified Svg.Setter.Text as Text
import qualified Svg.Setter.Use as Use
import qualified Test.Svg.Sample as Sample
import           Svg.Types.Core
import qualified Data.Text as T
import qualified Data.Map as Map

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
  print $ Right Default.rect >>= Rect.x 1
  print $ Right Default.rect >>= Circle.fill "black"
  assertEqual (Left "should be a circle instead of rect")
              (Right Default.rect >>= Circle.fill "black")
  assertEqual (Left "Attribute fill already set")
              (Right Default.rect >>= Rect.fill "black" >>= Rect.fill "red")

  assertEqual Sample.w3c $ fromRight $
    Right Default.svg
      >>= Svg.width 100
      >>= Svg.height 100
      >>= addChildren [
        fromRight $ Right Default.circle
                      >>= Circle.cx 50
                      >>= Circle.cy 50
                      >>= Circle.stroke "green"
                      >>= Circle.strokewidth 4
                      >>= Circle.fill "yellow"
      ]

test_sample_use = --do
   --let s = renderXMLElement Sample.heartWithShade
   --writeFile "/tmp/heart.svg" $ cs s
   --writeFile "/tmp/heart2.svg" $ cs $ renderXMLElement $
   assertEqual Sample.heartWithShade $
    fromRight $ Right Default.svg
      >>= Svg.viewBox (-40) 0 150 100
      >>= addChildren [
            fromRight $ Right Default.g
               >>= G.fill "grey"
               >>= G.transform [Rotate (-10) (50,100), Translate (-36) 45.5, SkewX 40, Scale 1 0.5]
               >>= addChildren [
                   fromRight $ Right Default.path
                      >>= Path.id "heart"
                      >>= Path.d [
                              M False 10 30
                            , A False 20 20 0 0 1 50 30
                            , A False 20 20 0 0 1 90 30
                            , Q False 90 60 50 90
                            , Q False 10 60 10 30
                            , Z True
                            ]
                    ]
          , fromRight $ Right Default.use
               >>= Use.href "#heart"
               >>= Use.fill "none"
               >>= Use.stroke "red"
       ]




printXMLElement :: Element -> IO()
printXMLElement element = putStrLn $ cs $ renderXMLElement element

renderXMLElement :: Element -> T.Text
renderXMLElement element = cs $ renderText def
  -- def { rsAttrOrder= const (toOrderedList ["x", "fill"])}
   $ doc element
    where doc root = Document {
                documentPrologue = Prologue {
                    prologueBefore = []
                  , prologueDoctype = Nothing
                  , prologueAfter = []
                  }
              , documentRoot = addAttribute root ("xmlns","http://www.w3.org/2000/svg")
              , documentEpilogue = []
              }


test_text =
   --let s = renderXMLElement Sample.grumpy
   --writeFile "/tmp/grumpy.svg" $ cs s
   assertEqual Sample.grumpy $ fromRight $ Right Default.svg
      >>= Svg.viewBox 0 0 240 80
      >>= addChildren [
          fromRight $ Right Default.style >>= addText style
        , text "small" 20 35 "My"
        , text "heavy" 40 35 "cat"
        , text "small" 55 55 "is"
        , text "Rrrrr" 65 55 "Grumpy!"
        ]
      where text c x y t = fromRight $ Right Default.text
                      >>= Text.class' [c]
                      >>= Text.x x
                      >>= Text.y y
                      >>= addText t

            style = "\
\          .small { font: italic 13px sans-serif; }\
\          .heavy { font: bold 30px sans-serif; }\
\\
\          /* Note that the color of the text is set with the    *\
\           * fill property, the color property is for HTML only */\
\          .Rrrrr { font: italic 40px serif; fill: red; }\
\        "


test_heart = assertEqual Sample.heart $ fromRight $
    Right Default.svg
      >>= Svg.viewBox 0 0 100 100
      >>= addChildren [
        fromRight $ Right Default.path
                      >>= Path.fill "none"
                      >>= Path.stroke "red"
                      >>= Path.d [
                              M False 10 30
                            , A False 20 20 0 0 1 50 30
                            , A False 20 20 0 0 1 90 30
                            , Q False 90 60 50 90
                            , Q False 10 60 10 30
                            , Z True
                            ]
        ]



addAttribute :: Element -> (String,T.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             Map.toList (elementAttributes element)
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }


