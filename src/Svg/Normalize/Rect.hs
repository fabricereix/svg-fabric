{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Rect where

--import Data.XML.Types (hasAttribute)
--import qualified Data.Map as Map
import           Data.String.Conversions
--import qualified Svg.DefaultElements as Default
--import qualified Data.Text as Text
--import           Text.XML
import qualified Svg.Types.Parser as Parser
--import           Svg.Types.Core
import           Svg.Types.Format


normalize :: String -> String -> Either String String
normalize "x" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute x in element rect"
normalize "y" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute y in element rect"
normalize "width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.pixel (cs v) of
          Right parsed -> Right $ formatPixel parsed
          Left _ -> case Parser.percentage (cs v) of
              Right parsed -> Right $ formatPercentage parsed
              Left _ -> case Parser.auto (cs v) of
                  Right parsed -> Right $ formatAuto parsed
                  Left _ ->  Left $ "Parsing error for attribute width in element rect"
normalize "height" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ -> case Parser.auto (cs v) of
              Right parsed -> Right $ formatAuto parsed
              Left _ ->  Left $ "Parsing error for attribute height in element rect"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill in element rect"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke in element rect"
normalize "stroke-width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute stroke-width in element rect"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
