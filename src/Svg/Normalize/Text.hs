{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Text where

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
          Left _ ->  Left $ "Parsing error for attribute x in element text"
normalize "y" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute y in element text"
normalize "dx" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute dx in element text"
normalize "dy" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute dy in element text"
normalize "class" v =
  case Parser.classes (cs v) of
      Right parsed -> Right $ formatClasses parsed
      Left _ ->  Left $ "Parsing error for attribute class in element text"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill in element text"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke in element text"
normalize "stroke-width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute stroke-width in element text"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
