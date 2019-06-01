{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Path where

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
normalize "d" v =
  case Parser.path (cs v) of
      Right parsed -> Right $ formatPath parsed
      Left _ ->  Left $ "Parsing error for attribute d"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill"
normalize "id" v =
  case Parser.id (cs v) of
      Right parsed -> Right $ formatId parsed
      Left _ ->  Left $ "Parsing error for attribute id"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke"
normalize "stroke-width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ -> case Parser.percentage (cs v) of
          Right parsed -> Right $ formatPercentage parsed
          Left _ ->  Left $ "Parsing error for attribute stroke-width"
normalize "transform" v =
  case Parser.transform (cs v) of
      Right parsed -> Right $ formatTransform parsed
      Left _ ->  Left $ "Parsing error for attribute transform"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
