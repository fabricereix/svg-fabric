{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Use where

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
normalize "href" v =
  case Parser.id (cs v) of
      Right parsed -> Right $ formatId parsed
      Left _ ->  Left $ "Parsing error for attribute href"
normalize "x" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ ->  Left $ "Parsing error for attribute x"
normalize "y" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ ->  Left $ "Parsing error for attribute y"
normalize "fill" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute fill"
normalize "stroke" v =
  case Parser.paint (cs v) of
      Right parsed -> Right $ formatPaint parsed
      Left _ ->  Left $ "Parsing error for attribute stroke"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
