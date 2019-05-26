{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.Parser where
import Svg.Types.Core
import Text.Parsec
import Data.Char
import Data.Maybe



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


points :: String -> Either Error Points
points = undefined

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

path :: String -> Either String Path
path = undefined

command' :: Stream s m Char => ParsecT s u m Command
command' = do
    c <- oneOf "MmLlHhVv"
    spaces
    lookup' (toUpper c) commandParsers (isLower c)

commandParsers ::Stream s m Char => [(Char, Bool->ParsecT s u m Command)]
commandParsers = [
   ('M', \relative -> do (x,y) <- coords
                         return $ M relative x y)
 , ('L', \relative -> do (x,y) <- coords
                         return $ L relative x y)
 , ('H', \relative -> H relative <$> double)
 , ('V', \relative -> V relative <$> double)
 ]

coords :: Stream s m Char => ParsecT s u m (Double,Double)
coords = do
    x <- double
    oneOf ", "
    spaces
    y <- double
    return (x, y)


lookup' :: Eq a => a -> [(a, b)] -> b
lookup' k m = fromMaybe (error "should not happen") (lookup k m)

classes :: String -> Either String Classes
classes = undefined


transform :: String -> Either String Transform
transform = undefined

id :: String -> Either String Id
id = undefined

contenttype :: String -> Either String ContentType
contenttype = undefined

number :: String -> Either Error Number
number _ = Left "TODO"

percentage :: String -> Either Error Percentage
percentage s = case parse (do{d<-double;char '%';eof; return d}) "" s of
  Left _ -> Left $ "Can not parse \"" ++ s ++ "\" to percentage"
  Right p -> Right $ Percentage p


viewbox :: String -> Either String Viewbox
viewbox s = case parse viewboxParser "" s of
  Left _ -> Left $ "Can not parse \"" ++ s ++ "\" to viewport"
  Right v -> Right v


viewboxParser :: Stream s m Char => ParsecT s u m Viewbox
viewboxParser = do
      minx <- double
      spaces
      miny <- double
      spaces
      w <- double
      spaces
      h <- double
      eof
      return $ Viewbox minx miny w h


auto :: String -> Either Error Auto
auto "auto" = Right AUTO
auto s      = Left $ "Can not parse \"" ++ s ++ "\" to auto"


removeFreeze :: String -> Either Error RemoveFreeze
removeFreeze "remove" = Right REMOVE
removeFreeze "freeze" = Right FREEZE
removeFreeze s = Left $ "Can not parse \"" ++ s ++ "\" to RemoveFreeze"


