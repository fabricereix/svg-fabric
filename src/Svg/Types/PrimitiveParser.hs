{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.PrimitiveParser where
import Svg.Types.Core
import Text.Parsec
import Data.Char
import Data.Maybe



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



xdouble' :: Stream s m Char => ParsecT s u m Double
xdouble' = rd <$> (plus <|> minus <|> n)
    where rd     = read :: String -> Double
          plus   = char '+' *> n
          minus  = (:) <$> char '-' <*> n
          n = many1 digit



lengthParser :: Stream s m Char => ParsecT s u m Length
lengthParser = do
    d <- double
    eof
    return $ Length d




command' :: Stream s m Char => ParsecT s (Maybe Char) m Command
command' = do
    c <- instruction
    spaces
    lookup' (toUpper c) commandParsers (isLower c)


instruction :: (Stream s m Char) => ParsecT s (Maybe Char) m Char
instruction = do c <- oneOf "MmLlHhVvZz"
                 modifyState $ const (Just c)
                 return c
           <|> do s <- getState
                  case s of
                    Nothing -> fail "missing instruction"
                    Just c -> return c



commandParsers ::Stream s m Char => [(Char, Bool->ParsecT s u m Command)]
commandParsers = [
   ('M', \relative -> do (x,y) <- coords
                         return $ M relative x y)
 , ('L', \relative -> do (x,y) <- coords
                         return $ L relative x y)
 , ('H', \relative -> H relative <$> double)
 , ('V', \relative -> V relative <$> double)
 , ('Z', return . Z)
 ]



basicTransform :: Stream s m Char => ParsecT s u m BasicTransform
basicTransform = do
    f <- string "matrix"
         <|> string "translate"
         <|> string "scale"
         <|> string "rotate"
         <|> string "skewx"
         <|> string  "skewy"
    spaces
    char '('
    spaces
    t <- lookup' f transformParsers
    char ')'
    spaces
    return t


transformParsers ::Stream s m Char => [(String, ParsecT s u m BasicTransform)]
transformParsers = [
   ("translate", do x <- double; spaces
                    (do y <- double
                        spaces
                        return $ Translate x y) <|> return (Translate x 0))
 , ("scale", do x <- double; spaces
                (do y <- double
                    spaces
                    return $ Scale x y) <|> return (Scale x x))
 , ("matrix", do a <- double; spaces
                 b <- double; spaces
                 c <- double; spaces
                 d <- double; spaces
                 e <- double; spaces
                 f <- double; spaces
                 return $ Matrix a b c d e f)
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





