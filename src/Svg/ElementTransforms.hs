{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Svg.ElementTransforms where

import qualified Data.Map                as Map
import           Data.String.Conversions
import qualified Data.Text               as T
import           Prelude                 hiding (id, length)
import qualified Svg.Attributes          as Attributes
import           Svg.Normalize
import           Svg.Types.Core
import           Svg.Types.Format
import           Svg.Types.Parser
import           Text.XML                hiding (readFile)



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
attributeExist element (name,_) = cs (nameLocalName name) `elem` Attributes.all (cs $ nameLocalName element)

defaultAttribute' :: Name -> (Name, T.Text) -> Bool
defaultAttribute' element attribute@(name, value) =
     attributeExist element attribute
  && not (defaultAttribute (cs $ nameLocalName element) (cs $ nameLocalName name) (cs value))


normalizeValue' :: Name -> (Name, T.Text) -> (Name, T.Text)
normalizeValue' element attribute@(
    name@Name{nameLocalName=attributeName}
  , attributeValue
  ) = if attributeExist element attribute
      then case normalize (cs (nameLocalName element)) (cs attributeName) (cs attributeValue) of
  Left e      -> error e
  Right value -> (name, cs value)
      else attribute




defaultAttribute :: String -> String -> String -> Bool
defaultAttribute element attribute value = case Attributes.defaultValue element attribute of
  Nothing -> False
  Just s  -> value == s



optimizePaths :: Element -> Element
optimizePaths = mapAttributes optimizePathD

optimizePathD ::  Name -> (Name, T.Text) -> (Name, T.Text)
optimizePathD _ (name@Name {nameLocalName="d"}, s) = (
    name
  , case path (cs s) of
      Left e  -> error e
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



