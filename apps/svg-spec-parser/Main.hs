{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

-- import           Data.Aeson (ToJSON,encode)
-- -- import qualified Data.Char as Char
-- -- import           Data.List
-- import           Data.String.Conversions
-- -- import           GHC.Generics
-- import           Svg.Spec.Core
-- import           Svg.Spec.Parser
-- import           Text.ParserCombinators.Parsec
--
-- instance ToJSON Spec
-- instance ToJSON Element
-- instance ToJSON Attribute
--
-- instance ToJSON AttributeDefault
-- instance ToJSON AttributeType


main :: IO()
main = do
    putStrLn "hello"
--   let presAttributeFilename =  "spec/presentation-attributes.spec"
--   let elemsFilename =  "spec/elements.spec"
--
--   presAttributes <- readFile presAttributeFilename
--   case parse attributesFile (cs presAttributeFilename) presAttributes of
--     Left e -> print e
--     Right _ -> do
--       -- print attrs
--       elems <- readFile elemsFilename
--       -- putStrLn elems
--       case parse elementsFile (cs elemsFilename) elems of
--         Left e -> print e
--         Right elements -> do
--            putStrLn $ cs $ encode $ Spec {_elements=elements}
