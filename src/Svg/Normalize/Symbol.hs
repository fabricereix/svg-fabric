{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Symbol where

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
normalize "width" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ ->  Left $ "Parsing error for attribute width"
normalize "height" v =
  case Parser.length (cs v) of
      Right parsed -> Right $ formatLength parsed
      Left _ ->  Left $ "Parsing error for attribute height"
normalize "viewBox" v =
  case Parser.viewbox (cs v) of
      Right parsed -> Right $ formatViewbox parsed
      Left _ ->  Left $ "Parsing error for attribute viewBox"
normalize "id" v =
  case Parser.id (cs v) of
      Right parsed -> Right $ formatId parsed
      Left _ ->  Left $ "Parsing error for attribute id"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
