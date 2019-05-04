{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.Parser where
import Svg.Types.Core
import Text.Parsec



--import Control.Applicative hiding ((<|>), many)



-- Primitive Parser
double :: Stream s m Char => ParsecT s u m Double
double = rd <$> (plus <|> minus <|> n)
    where rd     = read :: String -> Double
          plus   = char '+' *> n
          minus  = (:) <$> char '-' <*> n
          n = many1 digit



type Error = String

paint :: String -> Either Error Paint
paint s =  case parse (many1 anyChar) "" s of
              Left e -> Left (show e)
              Right d -> Right $ Color d

length :: String -> Either String Length
length s = case parse (do {d<-double;eof;return d}) "" s of
    Left e -> Left (show e)
    Right d -> Right $ Length d


percentage :: String -> Either Error Percentage
percentage _ = Left "TODO"

viewport :: String -> Either String Viewport
viewport = undefined

auto :: String -> Either Error Auto
auto "auto" = Right AUTO
auto s      = Left $ "Can not parse \"" ++ s ++ "\" to auto"

number :: String -> Either Error Number
number = undefined

removeFreeze :: String -> Either Error RemoveFreeze
removeFreeze "remove" = Right REMOVE
removeFreeze "freeze" = Right FREEZE
removeFreeze s = Left $ "Can not parse \"" ++ s ++ "\" to RemoveFreeze"


