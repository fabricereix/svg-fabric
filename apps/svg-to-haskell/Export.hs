{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Export where
import Data.Aeson
import Svg.Spec.Core


instance FromJSON AttributeDefault where
  parseJSON (Number 0) = return ZERO
  parseJSON (String "") = return EMPTY_STRING
  parseJSON (String "auto") = return AUTO
  parseJSON x = fail $  "Invalid Default " ++ show x

instance FromJSON AttributeType where
  parseJSON (String "auto") = return AUTO_TYPE
  parseJSON (String "length") = return LENGTH
  parseJSON (String "percentage") = return PERCENTAGE
  parseJSON (String "points") = return POINTS
  parseJSON x = fail $  "Invalid type " ++ show x

instance FromJSON Attribute where
  parseJSON = withObject "attribute" $ \o -> do
    _name <- o .: "name"
    _type <- o .: "type"
    _default <- o .: "default"
    return Attribute{..}

instance FromJSON Spec where
  parseJSON = withObject "spec" $ \o -> do
    _elements    <- o .: "elements"
    return Spec{..}

instance FromJSON Element where
  parseJSON = withObject "element" $ \o -> do
    _name    <- o .: "name"
    _attributes <- o .: "attributes"
    return Element{..}

