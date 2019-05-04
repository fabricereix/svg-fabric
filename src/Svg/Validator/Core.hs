module Svg.Validator.Core where

import Text.XML
import qualified Data.Text as Text

type AttributeName = String
data Error = InvalidElement Name
           | InvalidAttribute String Name
           | AttributeDefault String String
           | InvalidAttributeValue String String Text.Text
           | AttributeFormat String String Text.Text
           deriving (Show, Eq)

