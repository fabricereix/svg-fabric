{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.XML hiding (writeFile)
import Hilbert
import KnightTour
import Sierpinski
import Hanoi
import Data.String.Conversions


main :: IO()
main = do
  putStrLn "SVG Gallery"
  mapM_ writeDiagram Hilbert.diagrams
  mapM_ writeDiagram KnightTour.diagrams
  mapM_ writeDiagram Sierpinski.diagrams
  mapM_ writeDiagram Hanoi.diagrams


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

