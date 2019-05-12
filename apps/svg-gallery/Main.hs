{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.XML hiding (writeFile)
import Hilbert
import KnightTour
-- import System.IO.Unsafe
-- import Text.Pretty.Simple
-- import Control.Monad.Catch
-- import Data.Either
-- import Data.Map
-- import qualified Data.Text as T
import Data.String.Conversions
--import Svg.Elements
--import Svg.Types
--import Svg.Xml.Formatter


main :: IO()
main = do
  putStrLn "SVG Gallery"
  mapM_ writeDiagram Hilbert.diagrams
  mapM_ writeDiagram KnightTour.diagrams


writeDiagram :: (String, Element) -> IO()
writeDiagram (filename, root) = do
  let outputFile = "/tmp/" ++ filename
  putStrLn $ "writing to " ++ outputFile
  writeFile outputFile $ cs $ renderText def $ Document {
                documentPrologue = Prologue {
                    prologueBefore = []
                  , prologueDoctype = Nothing
                  , prologueAfter = []
                  }
              , documentRoot = addAttribute root ("xmlns","http://www.w3.org/2000/svg")
              , documentEpilogue = []
              }

addAttribute :: Element -> (String,Text.Text) -> Element
addAttribute element (attributeName,v) = element {
          elementAttributes=Map.fromList $
             Map.toList (elementAttributes element)
            ++ [(Name {nameLocalName=cs attributeName, nameNamespace=Nothing, namePrefix=Nothing}, v)]
      }

--
--   pPrint $ unsafePerformIO $ runX $ readString [] s
--
-- data MyException = ThisException | ThatException
--     deriving Show
--
-- instance Exception MyException
--
--
-- test1,test2 :: Either SomeException Document
--
-- test1 = parseText def "<user name=\"fab\"><children></children></user>"
-- test2 = parseText def "<a>"
--
--
--
--
-- doc1 :: Document
-- doc1 = Document {
--     documentPrologue = Prologue {
--         prologueBefore = []
--       , prologueDoctype = Nothing
--       , prologueAfter = []
--     }
--   , documentRoot = Element {
--         elementName = Name {
--             nameLocalName = "a"
--           , nameNamespace = Nothing
--           , namePrefix = Nothing
--           }
--       , elementAttributes = fromList []
--       , elementNodes = []
--       }
--   , documentEpilogue = []
--   }
--
--
-- type Attribute = (Name, T.Text)
--
--
-- evalAttribute :: Map T.Text Int -> Attribute -> Attribute
-- --evalAttribute _ attr@(Name {nameNamespace=Nothing}, _) = attr
-- evalAttribute vs (Name {nameLocalName=localName, nameNamespace=Just "xx"}, value) = (Name localName Nothing Nothing, eval vs value)
-- evalAttribute _ a = a
--
-- eval ::  Map T.Text Int -> T.Text -> T.Text
-- eval vs s = cs $ show $ vs ! s
--
--
-- values :: Map T.Text Int
-- values = fromList [("counter", 10)]
--
-- test10 :: Attribute
-- test10 = evalAttribute values (Name "attr" Nothing Nothing, "1")
--
-- test3 :: Document
-- test3 = fromRight doc1 test1
--
-- sample1 :: String
-- sample1 = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
--           \<svg>\
--           \    <circle cx=\"2\" cy=\"2\" r=\"1\" />\
--           \</svg>"
--
--
-- -- all attributes
-- -- recursive processing
-- -- formatLength :: Int -> X ->
-- -- formatLength n elem = elem
-- --
-- -- formatAttribute n attribute
-- --
-- -- format 0
--
-- --attribute1 = NTree ( XAttr "cx" ) [ NTree ( XText "50.5") []]
-- --attribute2 = NTree ( XAttr "cx" ) [ NTree ( XText "50") []]
--
--
-- -- Attributes are basic elements
-- -- Play with basic attribute type
--
-- helloWorld :: ArrowXml a => a XmlTree XmlTree
-- helloWorld = mkelem "html" []              -- (1)
--       [ mkelem "head" []
--         [ mkelem "title" []
--           [ txt "Hello World" ]     -- (2)
--         ]
--       , mkelem "body"
--         [ sattr "class" "haskell" ] -- (3)
--         [ mkelem "h1" []
--           [ txt "Hello World" ]     -- (4)
--         ]
--       ]
--
-- attr1 ::  ArrowXml a => a n XmlTree
-- attr1 = sattr "r" "1"
