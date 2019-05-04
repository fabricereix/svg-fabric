{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Svg.Validator where

import Test.Framework
import Data.String.Conversions
import Text.XML
import qualified Data.Map as Map
import           Svg.Validator
import           Svg.Validator.Core
import qualified Svg.Validator.Rect as Rect


-- default prevent explicit failure
fromRight :: Show l => Either l r -> r
fromRight (Left e) = error $ show e
fromRight (Right x) = x



xmlSvg0 = Element {
    elementName=Name { nameLocalName = "svg", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [("width","100"),("height","100")]
  , elementNodes = [
      NodeElement $ Element {
          elementName=Name { nameLocalName = "circle", nameNamespace = Nothing, namePrefix = Nothing}
        , elementAttributes = Map.fromList [("cx", "50"),("cy","50"),("fill","yellow")]
        , elementNodes = []
      }
    ]
  }

xmlRect0 = Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList []
  , elementNodes = [
    ]
  }

xmlRect1 = Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [("x", "10")]
  , elementNodes = []
  }
xmlRect2 = Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [("x", "10"),("fill","black")]
  , elementNodes = []
  }

xmlRectInvalid = Element {
    elementName=Name { nameLocalName = "rect", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [("x", "10"),("cx","0")]
  , elementNodes = []
  }

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
    where look k = case Map.lookup k m of
                        Nothing -> []
                        Just v  -> [(k, v)]

test_validate_elementName = do

   let name1 = Name { nameLocalName = "xxx", nameNamespace =     Nothing, namePrefix = Nothing}
   assertEqual [InvalidElement name1] $ validate $ Element {
       elementName=name1
     , elementAttributes=Map.fromList []
     , elementNodes = []}


   assertEqual [] $ validate xmlSvg0
   assertEqual [] $ validate xmlRect1


test_validate_attributes = do
   let name1 = Name { nameLocalName = "xxx", nameNamespace =     Nothing, namePrefix = Nothing}

   assertEqual [InvalidAttribute "rect" name1] $ Rect.validateAttribute ("xxx","1")
   assertEqual [AttributeDefault "rect" "x"] $ Rect.validateAttribute ("x","0")
   assertEqual [InvalidAttributeValue "rect" "x" "a"] $ Rect.validateAttribute ("x","a")
   assertEqual [AttributeFormat "rect" "x" "01"] $ Rect.validateAttribute ("x","01")
   assertEqual [] $ Rect.validateAttribute ("x","1")

   assertEqual [] $ Rect.validateAttributes $ Map.fromList [("x","1")]
   assertEqual [InvalidAttribute "rect" name1] $ Rect.validateAttributes $ Map.fromList [("x","1"),("xxx","xxx")]




test_validate_children = do

   let name1 = Name { nameLocalName = "xxx", nameNamespace =     Nothing, namePrefix = Nothing}
   assertEqual [InvalidElement name1] $ validate $ Element {
       elementName=Name { nameLocalName = "svg", nameNamespace =     Nothing, namePrefix = Nothing}
     , elementAttributes=Map.fromList []
     , elementNodes = [
         NodeElement $ Element {
             elementName=name1
           , elementAttributes = Map.fromList []
          , elementNodes = []
          }
    ]}


