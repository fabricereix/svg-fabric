{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.Parser where
import Svg.Types.Core
import Text.Parsec



--import Control.Applicative hiding ((<|>), many)



-- Primitive Parser
double :: Stream s m Char => ParsecT s u m Double
double = rd <$> (plus <|> minus <|> number)
    where rd     = read :: String -> Double
          plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 digit



type Error = String

paint :: String -> Either Error Paint
paint s =  case parse (do {many1 anyChar}) "" s of
              Left e -> Left (show e)
              Right d -> Right $ Color d

_length :: String -> Either String Length
_length s = case parse (do {d<-double;eof;return d}) "" s of
    Left e -> Left (show e)
    Right d -> Right $ Length d


percentage :: String -> Either Error Percentage
percentage _ = Left "TODO"