{-# LANGUAGE OverloadedStrings #-}
module Svg.Normalize.Animate where

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
normalize "fill" v =
  case Parser.removeFreeze (cs v) of
      Right parsed -> Right $ formatRemovefreeze parsed
      Left _ ->  Left $ "Parsing error for attribute fill in element animate"
normalize name _ = Left $ "Attribute " ++ name ++ " does not exist"
