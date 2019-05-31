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
   ('M', \relative -> do (x,y) <- coords; spaces
                         return $ M relative x y)
 , ('L', \relative -> do (x,y) <- coords; spaces
                         return $ L relative x y)
 , ('H', \relative -> do x <- double; spaces
                         return $ H relative x)
 , ('V', \relative -> do y <- double; spaces
                         return $ V relative y)
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

classParser :: Stream s m Char => ParsecT s u m String
classParser = do s <-many1 $ oneOf "-abcdefghijklmnopqrstuvwxyz0123456789"
                 spaces
                 return s

idParser :: Stream s m Char => ParsecT s u m String
idParser = do s <-many1 $ oneOf "abcdefghijklmnopqrstuvwxyz0123456789"
              spaces
              return s

point :: Stream s m Char => ParsecT s u m (Double,Double)
point = do x <- double
           many $ oneOf " ,";
           y <- double;
           many $ oneOf " ,";
           return (x,y)


transformParsers ::Stream s m Char => [(String, ParsecT s u m BasicTransform)]
transformParsers = [
   ("translate", do x <- double; spaces
                    (do y <- double; spaces
                        return $ Translate x y) <|> return (Translate x 0))
 , ("scale", do x <- double; spaces
                (do y <- double; spaces
                    return $ Scale x y) <|> return (Scale x x))
 , ("rotate", do a <- double; spaces
                 (do x <- double; spaces
                     y <- double; spaces
                     return $ Rotate a (x,y)) <|> return (Rotate a (0,0)))
 , ("skewX", do x <- double; spaces; return $ SkewX x)
 , ("skewX", do y <- double; spaces; return $ SkewY y)
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
      minx <- double; spaces
      miny <- double; spaces
      w <- double; spaces
      h <- double; spaces
      eof
      return $ Viewbox minx miny w h





