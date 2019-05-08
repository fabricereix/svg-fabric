module Hilbert where
import Text.XML
import qualified Svg.DefaultElements as Default
import qualified Svg.Setter.Svg as Svg

fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x

type Filename = String
diagrams :: [(Filename, Element)]
diagrams = [("hilbert.svg", fromRight $ Right Default.svg
                                 >>= Svg.width 300
                                 >>= Svg.height 300)
  ]
