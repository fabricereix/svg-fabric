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
import Svg.Types.Core
import Svg.Types.Format
import Svg.Types.Parser

import Prelude hiding (id, length)

--import qualified Prelude as P
--import Prelude()
-- hiding (id, length)
import qualified Svg.Attributes as Attributes





mapAttributes :: (Name -> (Name,T.Text)->(Name,T.Text))-> (Element->Element)
mapAttributes f Element {
    elementName=element
  , elementAttributes=attributes
  , elementNodes=children
  } = Element {
     elementName=element
   , elementAttributes=Map.fromList $ map (f element) $ Map.toList  attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ mapAttributes f element2
                             x -> x) children
   }

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
normalizeValue = mapAttributes normalizeValue'



attributeExist :: Name -> (Name, T.Text) -> Bool
attributeExist Name{nameLocalName=element} (Name{nameLocalName=attributeName},_) = cs attributeName `elem` Attributes.all (cs element)

defaultAttribute' :: Name -> (Name, T.Text) -> Bool
defaultAttribute' Name{nameLocalName=element} (Name{nameLocalName=attributeName},value) = not $ defaultAttribute (cs element) (cs attributeName) (cs value)

normalizeValue' :: Name -> (Name, T.Text) -> (Name, T.Text)
normalizeValue' element attribute@(
    name@Name{nameLocalName=attributeName}
  , attributeValue
  ) = if attributeExist element attribute
      then case normalizeAttributeValue (cs (nameLocalName element)) (cs  attributeName) (cs attributeValue) of
  Left e -> error e
  Right value -> (name, cs value)
      else attribute



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
optimizePaths = mapAttributes optimizePathD

optimizePathD ::  Name -> (Name, T.Text) -> (Name, T.Text)
optimizePathD _ (name@Name {nameLocalName="d"}, s) = (
    name
  , case path (cs s) of
      Left e -> error e
      Right v -> cs $ formatPath $ optimizePath' v
  )
optimizePathD _ attribute = attribute


optimizePath' :: Path -> Path
optimizePath' (Path xs) = Path $ optimizePath xs


optimizePath :: [Command] -> [Command]
optimizePath [] = []
optimizePath [c] = [c]
optimizePath (c1:c2:css) = case mergeCommands c1 c2 of
   [_,_] -> c1:optimizePath (c2:css)
   c     -> optimizePath (c ++ css)


mergeCommands :: Command -> Command -> [Command]
mergeCommands c1@(H True x1) c2@(H True x2) = if signum x1 == signum x2
                                              then [H True (x1+x2)]
                                              else [c1,c2]
mergeCommands c1@(V True x1) c2@(V True x2) = if signum x1 == signum x2
                                              then [V True (x1+x2)]
                                              else [c1,c2]

mergeCommands (M True x1 y1) (M True x2 y2) = [M True (x1+x2) (y1+y2)]
mergeCommands c1 c2 = [c1, c2]



