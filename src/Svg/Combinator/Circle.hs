module Svg.Combinator.Circle where
import Svg.Elements
import qualified Svg.Setter.Circle as Circle
import qualified Svg.Types.Parser as Parser



cx :: String -> Element -> Either String Element
cx v element@(Circle _ _ _ _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Circle.cx element $ OneOf2 parsed
      Left _ -> case Parser.percentage v of
          Right parsed -> Right $ Circle.cx element $ TwoOf2 parsed
          Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute cx"
cx _ _ = error "should have a Circle element!"

cy :: String -> Element -> Either String Element
cy v element@(Circle _ _ _ _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Circle.cy element $ OneOf2 parsed
      Left _ -> case Parser.percentage v of
          Right parsed -> Right $ Circle.cy element $ TwoOf2 parsed
          Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute cy"
cy _ _ = error "should have a Circle element!"

r :: String -> Element -> Either String Element
r v element@(Circle _ _ _ _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Circle.r element $ OneOf2 parsed
      Left _ -> case Parser.percentage v of
          Right parsed -> Right $ Circle.r element $ TwoOf2 parsed
          Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute r"
r _ _ = error "should have a Circle element!"

pathLength :: String -> Element -> Either String Element
pathLength v element@(Circle _ _ _ _ _ _ _) =
  case Parser.number v of
      Right parsed -> Right $ Circle.pathLength element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute pathLength"
pathLength _ _ = error "should have a Circle element!"

fill :: String -> Element -> Either String Element
fill v element@(Circle _ _ _ _ _ _ _) =
  case Parser.paint v of
      Right parsed -> Right $ Circle.fill element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute fill"
fill _ _ = error "should have a Circle element!"

stroke :: String -> Element -> Either String Element
stroke v element@(Circle _ _ _ _ _ _ _) =
  case Parser.paint v of
      Right parsed -> Right $ Circle.stroke element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value \"" ++ v ++ "\" for attribute stroke"
stroke _ _ = error "should have a Circle element!"

