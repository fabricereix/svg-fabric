module Svg.Validator.Core where

import Text.XML

type AttributeName = String
data Error = InvalidElement Name
           | InvalidAttribute String Name
           | AttributeDefault String String
           deriving (Show, Eq)

