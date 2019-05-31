{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase   #-}
module Svg.ElementTransforms where

-- import qualified Data.ByteString.Char8         as B
import           Data.String.Conversions
---import           Data.Text.Encoding
import Text.XML hiding (readFile)
import qualified Data.Map as Map
import qualified Data.Text as T

-- import qualified Svg.Setter.Svg as SvgSetter
-- import Svg.Types.Core
import Svg.Types.Format
import Svg.Types.Parser

import Prelude hiding (id, length)

--import qualified Prelude as P
--import Prelude()
-- hiding (id, length)





mapAttributes :: (Name -> (Name,T.Text)->(Name,T.Text))-> (Element->Element)
mapAttributes = undefined

filterAttributes :: (Name -> (Name,T.Text)->Bool)-> (Element->Element)
filterAttributes predicate Element {
    elementName=element
  , elementAttributes=attributes
  , elementNodes=children
  } = Element {
     elementName=element
   , elementAttributes=Map.fromList $ filter (predicate element) $ Map.toList  attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ filterAttributes predicate element2
                             x -> x) children
   }


removeDefault :: Element -> Element
removeDefault = filterAttributes defaultAttribute'

removeUnknown :: Element -> Element
removeUnknown = filterAttributes attributeExist


normalizeValue :: Element -> Element
normalizeValue element@Element {
    elementName=name
  , elementAttributes=attributes
  , elementNodes=children
  } = element {
   elementAttributes=Map.fromList $ map (normalizeValue' name) $ Map.toList attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ normalizeValue element2
                             x -> x) children
   }


attributeExist :: Name -> (Name, T.Text) -> Bool
attributeExist Name{nameLocalName=element} (Name{nameLocalName=attributeName},_) = exists (cs element) (cs attributeName)

defaultAttribute' :: Name -> (Name, T.Text) -> Bool
defaultAttribute' Name{nameLocalName=element} (Name{nameLocalName=attributeName},value) = not $ defaultAttribute (cs element) (cs attributeName) (cs value)

normalizeValue' :: Name -> (Name, T.Text) -> (Name, T.Text)
normalizeValue' Name{nameLocalName=element} attribute@(name@Name{nameLocalName=attributeName}, attributeValue) = if exists (cs element) (cs attributeName)
                          then case normalizeAttributeValue (cs element) (cs  attributeName) (cs attributeValue) of
  Left e -> error e
  Right value -> (name, cs value)
                          else attribute


exists :: String -> String -> Bool
exists _ "unknown" = error "got unknown!!"
exists "svg" "viewBox" = True
exists "circle" attributeName = circleExists attributeName
exists "path" attributeName = pathExists attributeName
exists _ _ = False

circleExists :: String -> Bool
circleExists "cx" = True
circleExists "cy" = True
circleExists "fill" = True
circleExists "r" = True
circleExists _ = False

pathExists :: String -> Bool
pathExists "d" = True
pathExists "stroke" = True
pathExists "stroke-width" = True
pathExists "unknown" = error "got unknown!!"
pathExists _ = False

circleDefault :: String -> Maybe String
circleDefault "cx" = Just "0"
circleDefault "cy" = Just "0"
circleDefault "r" = Nothing
circleDefault "fill" = Nothing
circleDefault name = error $ "invalid attribute " ++ name


defaultAttribute :: String -> String -> String -> Bool
defaultAttribute "svg" _ _ = False
defaultAttribute "circle" name value = case circleDefault name of
  Nothing -> False
  Just s -> value == s
defaultAttribute _ _ _ = False

svgNormalizeValue :: String -> String -> Either String String
svgNormalizeValue "viewBox" s = case viewbox s of
  Left e -> Left e
  Right v -> Right $ formatViewbox v
svgNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist for svg"


circleNormalizeValue :: String -> String -> Either String String
circleNormalizeValue "cx" s = case length s of
         Left e -> Left e
         Right v -> Right $ formatLength v
circleNormalizeValue "cy" s = case length s of
         Left e -> Left e
         Right v -> Right $ formatLength v
circleNormalizeValue "r" s = case length s of
         Left e -> Left e
         Right v -> Right $ formatLength v
circleNormalizeValue "fill" s = case paint s of
         Left e -> Left e
         Right v -> Right $ formatPaint v
circleNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist for circle"

pathNormalizeValue :: String -> String -> Either String String
pathNormalizeValue "d" s = case path s of
         Left e -> Left e
         Right v -> Right $ formatPath v
pathNormalizeValue "stroke" s = case paint s of
         Left e -> Left e
         Right v -> Right $ formatPaint v
pathNormalizeValue "stroke-width" s = case length s of
         Left e -> Left e
         Right v -> Right $ formatLength v
pathNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist for path"


normalizeAttributeValue :: String -> String -> String -> Either String String
normalizeAttributeValue "svg" name value = svgNormalizeValue name value
normalizeAttributeValue "circle" name value = circleNormalizeValue name value
normalizeAttributeValue "path" name value = pathNormalizeValue name value
normalizeAttributeValue name _ _ = error $ "invalid element " ++ name




optimizePaths :: Element -> Element
optimizePaths element@Element{elementAttributes=attributes}
  = element{elementAttributes=Map.fromList $ Map.toList attributes ++[newAttrib]}
   where newAttrib = (Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}, "x")





