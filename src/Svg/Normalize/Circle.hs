{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Circle where

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
normalize "cx" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute cx in element circle"
normalize "cy" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute cy in element circle"
normalize "r" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute r in element circle"
normalize "pathLength" v =
  case Parser.number (cs v) of
      Right parsed -> Right $ formatNumber parsed
      Left _ ->  Left $ "Parsing error for attribute pathLength in element circle"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill in element circle"
normalize "id" v =
  case Parser.id (cs v) of
      Right parsed -> Right $ formatId parsed
      Left _ ->  Left $ "Parsing error for attribute id in element circle"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke in element circle"
normalize "stroke-width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute stroke-width in element circle"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
