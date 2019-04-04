module Svg.Types.Parser where
import Svg.Types.Core


type Error = String

paint :: String -> Either Error Paint
paint _ = Left "TODO"

length :: String -> Either Error Length
length _ = Left "TODO"

percentage :: String -> Either Error Percentage
percentage _ = Left "TODO"