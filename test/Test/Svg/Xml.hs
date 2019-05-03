{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Xml where

import Test.Framework
import Svg.Elements
import Svg.Attribute
import qualified Svg.Combinator.Rect as Rect
import qualified Svg.Combinator.Circle as Circle
import qualified Svg.Combinator.Svg as Svg
-- in fact don't like the default fromRight
-- import Data.Either
import Data.String.Conversions
import qualified Text.XML as XML
import Text.XML hiding (Element)
import qualified Data.Map as Map
--import qualified Data.Text as Text
--import Text.XML.Stream.Render
import Svg.Parser

-- default prevent explicit failure
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x


rect0, rect1, rect2 :: Element
rect0 = defaultRect
rect1 = fromRight $ Right defaultRect >>= Rect.x "10"
rect2 = fromRight $ Right defaultRect >>= Rect.x "10" >>= Rect.fill "black"

circle1 = fromRight $ Right defaultCircle >>= Circle.r "1"


svg0 :: Element
svg0 = (fromRight $ Right defaultSvg
           >>= Svg.width "100"
           >>= Svg.height "100")
       `addChildren` [
           fromRight $ Right defaultCircle
               >>= Circle.cx "50"
               >>= Circle.cy "50"
               >>= Circle.r "40"
               >>= Circle.fill "yellow"
       ]
-- <svg width="100" height="100">
--   <circle cx="50" cy="50" r="40" fill="yellow" />
-- </svg>


xmlRect0 = XML.Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList []
  , elementNodes = []
  }
xmlRect1 = XML.Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList $ [("x", "10")]
  , elementNodes = []
  }
xmlRect2 = XML.Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList $ [("x", "10"),("fill","black")]
  , elementNodes = []
  }

xmlRectInvalid = XML.Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList $ [("x", "10"),("cx","0")]
  , elementNodes = []
  }

toXML :: Element -> XML.Element
toXML element = XML.Element {
    elementName=Name { nameLocalName = cs (name element), nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList $ map (\(k,v)-> (Name (cs k) Nothing Nothing,cs v)) $ attributes element
  , elementNodes = map (NodeElement . toXML) $ children element
  }

fromXML :: XML.Element -> Either String Element
fromXML (XML.Element {
    elementName=Name { nameLocalName = n}
  , elementAttributes = attrs
  , elementNodes = []
  }) = parse (cs n) $ map (\(k, v)->(cs (nameLocalName k), cs v)) $  Map.toList attrs
fromXML _ = undefined



printXMLElement :: XML.Element -> IO()
printXMLElement element = putStrLn $ cs $ renderText def
  -- def { rsAttrOrder= const (toOrderedList ["x", "fill"])}
   $ doc element
    where doc root = Document {
                documentPrologue = Prologue {
                    prologueBefore = []
                  , prologueDoctype = Nothing
                  , prologueAfter = []
                  }
              , documentRoot = root
              , documentEpilogue = []
              }


toOrderedList :: (Ord k) => [k] -> Map.Map k a -> [(k, a)]
toOrderedList keys m = concatMap look keys
    where look k = case (Map.lookup k m) of
                        Nothing -> []
                        Just v  -> [(k, v)]

test_encode = do

  putStrLn $ show $ defaultRect



--toList :: Map k a -> [(k, a)] Source#
--toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]



