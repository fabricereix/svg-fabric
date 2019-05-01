{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Xml where

import Test.Framework
# import Svg.Playground.Elements hiding(elementName)
import Svg.Elements
import Svg.Attribute
import qualified Svg.Combinator.Rect as Rect
import qualified Svg.Combinator.Circle as Circle
import Data.Either
import Data.String.Conversions
import qualified Text.XML as XML
import Text.XML hiding (Element)
import qualified Data.Map as Map
--import qualified Data.Text as Text
--import Text.XML.Stream.Render

rect0, rect1, rect2 :: Element
rect0 = defaultRect
rect1 = fromRight defaultRect $ (Right defaultRect) >>= Rect.x "10"
rect2 = fromRight defaultRect $ (Right defaultRect) >>= Rect.x "10"
                                                    >>= Rect.fill "black"

circle1 = fromRight defaultCircle $ (Right defaultCircle) >>= Circle.r "1"


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

toXML :: Element -> XML.Element
toXML element = XML.Element {
    elementName=Name { nameLocalName = cs (name element), nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList $ map (\(k,v)-> (Name (cs k) Nothing Nothing,cs v)) $ attributes element
  , elementNodes = []
  }


printXMLElement :: XML.Element -> IO()
printXMLElement element = putStrLn $ cs $ renderText (def {
  rsAttrOrder= const (toOrderedList ["x", "fill"])
  }) $ doc element
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



