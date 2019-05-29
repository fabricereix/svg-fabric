{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Svg.Types.Parser where
import Svg.Types.Core
import Svg.Types.PrimitiveParser
import Text.Parsec
--import Data.Char
--import Data.Maybe


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


path :: String -> Either String Path
path s = case runParser (many command') Nothing "" s of
  Left e   -> Left (show e)
  Right xs -> Right $ Path xs

classes :: String -> Either String Classes
classes = undefined


transform :: String -> Either String Transform
transform s = case parse (do ts <-many basicTransform
                             spaces
                             eof
                             return ts) "" s of
  Left e -> Left $ "Invalid transform value - " ++ show e
  Right xs -> Right $ Transform xs


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


auto :: String -> Either Error Auto
auto "auto" = Right AUTO
auto s      = Left $ "Can not parse \"" ++ s ++ "\" to auto"


removeFreeze :: String -> Either Error RemoveFreeze
removeFreeze "remove" = Right REMOVE
removeFreeze "freeze" = Right FREEZE
removeFreeze s = Left $ "Can not parse \"" ++ s ++ "\" to RemoveFreeze"


