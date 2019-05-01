module Svg.Elements where

import Svg.Types.Core


data OneOf1 a = OneOf1 a
                deriving (Eq,Show)

data OneOf2 a b = OneOf2 a | TwoOf2 b
                  deriving (Eq,Show)

data OneOf3 a b c = OneOf3 a | TwoOf3 b | ThreeOf3 c
                  deriving (Eq,Show)


data Element =
    Animate
      [Element]  -- children
      (OneOf1 RemoveFreeze) -- fill

  | Circle
      [Element]  -- children
      (OneOf2 Length Percentage) -- cx
      (OneOf2 Length Percentage) -- cy
      (OneOf2 Length Percentage) -- r
      (Maybe (OneOf1 Number)) -- pathLength
      (Maybe (OneOf1 Paint)) -- fill

  | Rect
      [Element]  -- children
      (OneOf2 Length Percentage) -- x
      (OneOf2 Length Percentage) -- y
      (OneOf3 Auto Length Percentage) -- width
      (OneOf3 Auto Length Percentage) -- height
      (Maybe (OneOf1 Paint)) -- fill

  | Svg
      [Element]  -- children
      (Maybe (OneOf1 Length)) -- width
      (Maybe (OneOf1 Length)) -- height
      (Maybe (OneOf1 Viewport)) -- viewport

  deriving (Show, Eq)


children :: Element -> [Element]
children (Animate cs _) = cs
children (Circle cs _ _ _ _ _) = cs
children (Rect cs _ _ _ _ _) = cs
children (Svg cs _ _ _) = cs



name :: Element -> String
name (Animate _ _) = "animate"
name (Circle _ _ _ _ _ _) = "circle"
name (Rect _ _ _ _ _ _) = "rect"
name (Svg _ _ _ _) = "svg"



defaultAnimate :: Element
defaultAnimate = Animate
  [] -- children
  (OneOf1 REMOVE) -- fill

defaultCircle :: Element
defaultCircle = Circle
  [] -- children
  (OneOf2 (Length 0)) -- cx
  (OneOf2 (Length 0)) -- cy
  (OneOf2 (Length 0)) -- r
  Nothing  -- pathLength
  Nothing  -- fill

defaultRect :: Element
defaultRect = Rect
  [] -- children
  (OneOf2 (Length 0)) -- x
  (OneOf2 (Length 0)) -- y
  (OneOf3 AUTO) -- width
  (OneOf3 AUTO) -- height
  Nothing  -- fill

defaultSvg :: Element
defaultSvg = Svg
  [] -- children
  Nothing  -- width
  Nothing  -- height
  Nothing  -- viewport

