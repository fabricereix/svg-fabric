{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Style where

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
normalize "type'" v =
  case Parser.contenttype (cs v) of
      Right parsed -> Right $ formatContenttype parsed
      Left _ ->  Left $ "Parsing error for attribute type' in element style"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
