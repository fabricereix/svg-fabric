{-# LANGUAGE OverloadedStrings #-}
module Svg.Combinator.Animate where
import Text.XML
import Data.String.Conversions
import qualified Svg.DefaultElements as Default


fill :: String -> Element -> Either String Element
fill _ Element {
    elementName=Name { nameLocalName="animate" }
  } = Right Default.animate
fill _ Element {
    elementName=Name { nameLocalName=name }
  } = Left $ (cs name) ++ " element - should be a animate element"



