{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.Parser where
import Svg.Types.Core
import Text.Parsec



--import Control.Applicative hiding ((<|>), many)



-- Primitive Parser
natural :: (Stream s m Char) => ParsecT s u m Int
natural = (do xs <- many1 digit
              return $ read xs) <?> "natural"


integer :: (Stream s m Char) => ParsecT s u m Int
integer = (do
    s <- sign
    i <- natural
    return $ s * i) <?> "integer"


sign :: (Stream s m Char) => ParsecT s u m Int
sign = do
    s <- option '+' $ oneOf "+-"
    return $ if s == '+' then 1 else -1


double :: (Stream s m Char) => ParsecT s u m Double
double =  do s <- sign
             i <- natural
             d <- option 0 _decimal
             return $ fromIntegral s * (fromIntegral i + d)
            <?> "double"

_decimal :: (Stream s m Char) => ParsecT s u m Double
_decimal = do
  char '.'
  xs <- many1 digit
  return $ read ("0." ++ xs)



double' :: Stream s m Char => ParsecT s u m Double
double' = rd <$> (plus <|> minus <|> n)
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
length s = case parse (do{d<-double;eof;return d}) "" s of
  Left _ -> Left "xx"
  Right l -> Right $ Length l

lengthParser :: Stream s m Char => ParsecT s u m Length
lengthParser = do
    d <- double
    eof
    return $ Length d


number :: String -> Either Error Number
number _ = Left "TODO"

percentage :: String -> Either Error Percentage
percentage _ = Left "TODO"

viewport :: String -> Either String Viewport
viewport s = case parse viewportParser "" s of
  Left _ -> Left $ "Can not parse \"" ++ s ++ "\" to viewport"
  Right v -> Right v


viewportParser :: Stream s m Char => ParsecT s u m Viewport
viewportParser = do
      minx <- double
      spaces
      miny <- double
      spaces
      w <- double
      spaces
      h <- double
      eof
      return $ Viewport minx miny w h


auto :: String -> Either Error Auto
auto "auto" = Right AUTO
auto s      = Left $ "Can not parse \"" ++ s ++ "\" to auto"


removeFreeze :: String -> Either Error RemoveFreeze
removeFreeze "remove" = Right REMOVE
removeFreeze "freeze" = Right FREEZE
removeFreeze s = Left $ "Can not parse \"" ++ s ++ "\" to RemoveFreeze"


