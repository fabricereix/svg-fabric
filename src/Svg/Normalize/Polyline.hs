{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Polyline where

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
normalize "points" v =
  case Parser.points (cs v) of
      Right parsed -> Right $ formatPoints parsed
      Left _ ->  Left $ "Parsing error for attribute points in element polyline"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill in element polyline"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke in element polyline"
normalize "stroke-width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute stroke-width in element polyline"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
