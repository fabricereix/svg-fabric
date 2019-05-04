module Svg.Combinator.Rect where
import Svg.Elements
import qualified Svg.Setter.Rect as Rect
import qualified Svg.Types.Parser as Parser



x :: String -> Element -> Either String Element
x v element@(Rect _ _ _ _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Rect.x element $ OneOf2 parsed
      Left _ -> case Parser.percentage v of
          Right parsed -> Right $ Rect.x element $ TwoOf2 parsed
          Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute x"
x _ _ = error "should have a Rect element!"

y :: String -> Element -> Either String Element
y v element@(Rect _ _ _ _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Rect.y element $ OneOf2 parsed
      Left _ -> case Parser.percentage v of
          Right parsed -> Right $ Rect.y element $ TwoOf2 parsed
          Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute y"
y _ _ = error "should have a Rect element!"

width :: String -> Element -> Either String Element
width v element@(Rect _ _ _ _ _ _ _) =
  case Parser.auto v of
      Right parsed -> Right $ Rect.width element $ OneOf3 parsed
      Left _ -> case Parser.length v of
          Right parsed -> Right $ Rect.width element $ TwoOf3 parsed
          Left _ -> case Parser.percentage v of
              Right parsed -> Right $ Rect.width element $ ThreeOf3 parsed
              Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute width"
width _ _ = error "should have a Rect element!"

height :: String -> Element -> Either String Element
height v element@(Rect _ _ _ _ _ _ _) =
  case Parser.auto v of
      Right parsed -> Right $ Rect.height element $ OneOf3 parsed
      Left _ -> case Parser.length v of
          Right parsed -> Right $ Rect.height element $ TwoOf3 parsed
          Left _ -> case Parser.percentage v of
              Right parsed -> Right $ Rect.height element $ ThreeOf3 parsed
              Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute height"
height _ _ = error "should have a Rect element!"

fill :: String -> Element -> Either String Element
fill v element@(Rect _ _ _ _ _ _ _) =
  case Parser.paint v of
      Right parsed -> Right $ Rect.fill element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute fill"
fill _ _ = error "should have a Rect element!"

stroke :: String -> Element -> Either String Element
stroke v element@(Rect _ _ _ _ _ _ _) =
  case Parser.paint v of
      Right parsed -> Right $ Rect.stroke element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute stroke"
stroke _ _ = error "should have a Rect element!"

