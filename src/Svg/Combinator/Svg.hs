module Svg.Combinator.Svg where
import Svg.Elements
import qualified Svg.Setter.Svg as Svg
import qualified Svg.Types.Parser as Parser



width :: String -> Element -> Either String Element
width v element@(Svg _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Svg.width element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value " ++ v
width _ _ = error "should have a Svg element!"

height :: String -> Element -> Either String Element
height v element@(Svg _ _ _ _) =
  case Parser.length v of
      Right parsed -> Right $ Svg.height element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value " ++ v
height _ _ = error "should have a Svg element!"

viewport :: String -> Element -> Either String Element
viewport v element@(Svg _ _ _ _) =
  case Parser.viewport v of
      Right parsed -> Right $ Svg.viewport element $ Just $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value " ++ v
viewport _ _ = error "should have a Svg element!"

