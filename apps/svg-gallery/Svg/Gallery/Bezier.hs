module Svg.Gallery.Bezier where
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Path     as Path
import qualified Svg.Setter.Svg      as Svg
import           Text.XML
-- import qualified Svg.Setter.G as G
import           Svg.Setter
-- import           Svg.Types.Core
import           Svg.Gallery.Helper
import Svg.Types.Core


-- diagram
type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [("bezier-flatness.svg", diagram)]


diagram :: Element
diagram = fromRight $ Right Default.svg
            >>= Svg.width 500
            >>= Svg.height 500
            >>= Svg.viewBox (-0.5) (-0.5) 3 2
            >>= Svg.stroke "black"
            >>= Svg.strokewidth 0.01
            >>= addChildren [
                  bezier
                ]

bezier :: Element
bezier = fromRight $ Right Default.path
            -- >>= Path.strokewidth 0.1
            >>= Path.stroke "black"
            >>= Path.fill "none"
            >>= Path.d (M False 0 0:[C False 0 1 1 1 1 0])




