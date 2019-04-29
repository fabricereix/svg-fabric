module Svg.Combinator.Animate where
import Svg.Elements
import qualified Svg.Setter.Animate as Animate
import qualified Svg.Types.Parser as Parser



fill :: String -> Element -> Either String Element
fill v element@(Animate _ _) =
  case Parser.removeFreeze v of
      Right parsed -> Right $ Animate.fill element $ OneOf1 parsed
      Left _ -> Left $ "Can not parse value " ++ v
fill _ _ = error "should have a Animate element!"

