module Svg.Playground.Sample where

import Svg.Types.Core
import Svg.Playground.Elements
import qualified Svg.Playground.Getter.Rect as GetterRect
import qualified Svg.Playground.Getter.Animate as GetterAnimate
import qualified Svg.Playground.Setter.Rect as SetterRect
import qualified Svg.Playground.Dump.Rect as DumpRect

import qualified Svg.Playground.Parser.Rect as ParserRect
import qualified Svg.Playground.Combinator.Rect as CombinatorRect
import Svg.Playground.Parser
import Svg.Playground.Dump.Element

test_attributes :: IO()
test_attributes = do
  let rect1 = Rect
                  (OneOf2 (Length 10))
                  (Just (OneOf1 (Color "black")))
  print $ attributes rect1
  print $ attributes $ Rect (OneOf2 (Length 10)) Nothing
  print $ attributes defaultRect -- always return empty list => generate even the unit test?
  print $ attributes (Animate (OneOf1 REMOVE))
  print $ attributes (Animate (OneOf1 FREEZE))



test_dump :: IO()
test_dump = do
  let rect1 = Rect
                  (OneOf2 (Length 10))
                  (Just (OneOf1 (Color "black")))
  print $ DumpRect.x rect1
  print $ DumpRect.fill $ Rect (OneOf2 (Length 10)) Nothing

test_getter :: IO()
test_getter = do
  let rect1 = Rect
                (OneOf2 (Length 10))
                (Just (OneOf1 (Color "black")))
  let animate1 = Animate
                   (OneOf1 REMOVE)
  print $ GetterRect.fill rect1
  print $ GetterAnimate.fill animate1
  print $ GetterAnimate.fill rect1


test_setter :: IO()
test_setter = do
  print $ defaultRect `SetterRect.x`    (TwoOf2 (Percentage 0.5))
                      `SetterRect.fill` (Just (OneOf1 (Color "black")))


test_combinator :: IO()
test_combinator = do
    print $ (Right defaultRect) >>= CombinatorRect.x "10"
                                >>= CombinatorRect.fill "black"
    print $ (Right defaultRect) >>= CombinatorRect.x "x"
    print $ (Right defaultAnimate) >>= CombinatorRect.x "10"


test_parser :: IO()
test_parser = do
  print $ (Right defaultRect) >>= ParserRect.attribute ("x", "10")
                              >>= ParserRect.attribute ("fill", "black")

  print $ ParserRect.attributes []
  print $ ParserRect.attributes [("x", "10"),("fill", "black")]
  print $ ParserRect.attributes [("x", "10"),("z", "20")]

  print $ parse "rect" [("x", "10"),("fill", "black")]






