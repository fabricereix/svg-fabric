module Svg.Gallery.Sunflower where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg
-- import qualified Svg.Setter.Rect as Rect
-- import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import Helper


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = map (\n-> ("sunflower" ++ show n ++ ".svg", diagram n)) [10]



diagram :: Int -> Element
diagram _ = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-20) (-20) 40 40
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.05
            >>= addChildren []






