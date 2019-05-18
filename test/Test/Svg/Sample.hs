{-# LANGUAGE OverloadedStrings #-}
module Test.Svg.Sample where

import qualified Data.Map as Map
import           Text.XML
import qualified Data.Text as T


simpleName :: T.Text -> Name
simpleName s = Name {nameLocalName = s, nameNamespace = Nothing, namePrefix = Nothing}

svgName :: T.Text -> Name
svgName s = Name {nameLocalName = s, nameNamespace = Nothing, namePrefix = Nothing}

-- Sample W3C
-- <svg width="100" height="100">
--  <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
-- </svg>
w3c = Element {
    elementName = Name {nameLocalName = "svg", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [
      (Name {nameLocalName = "width", nameNamespace = Nothing, namePrefix = Nothing},"100")
    , (Name {nameLocalName = "height", nameNamespace = Nothing, namePrefix = Nothing},"100")
    ]
  , elementNodes = [
      NodeElement $ Element {
        elementName = Name {nameLocalName = "circle", nameNamespace = Nothing, namePrefix = Nothing}
      , elementAttributes = Map.fromList [
          (Name {nameLocalName = "cx", nameNamespace = Nothing, namePrefix = Nothing},"50")
        , (Name {nameLocalName = "cy", nameNamespace = Nothing, namePrefix = Nothing},"50")
        , (Name {nameLocalName = "stroke", nameNamespace = Nothing, namePrefix = Nothing},"green")
        , (Name {nameLocalName = "stroke-width", nameNamespace = Nothing, namePrefix = Nothing},"4")
        , (Name {nameLocalName = "fill", nameNamespace = Nothing, namePrefix = Nothing},"yellow")
        ]
      , elementNodes = []
      }
    ]
  }


-- <svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
--  <path fill="none" stroke="red"
--    d="M 10,30
--       A 20,20 0,0,1 50,30
--       A 20,20 0,0,1 90,30
--       Q 90,60 50,90
--       Q 10,60 10,30 z" />
-- </svg>
heart = Element {
    elementName = Name {nameLocalName = "svg", nameNamespace = Nothing, namePrefix = Nothing}
  , elementAttributes = Map.fromList [
      (Name {nameLocalName = "viewBox", nameNamespace = Nothing, namePrefix = Nothing},"0 0 100 100")
    ]
  , elementNodes = [
      NodeElement $ Element {
        elementName = Name {nameLocalName = "path", nameNamespace = Nothing, namePrefix = Nothing}
      , elementAttributes = Map.fromList [
          (Name {nameLocalName = "fill", nameNamespace = Nothing, namePrefix = Nothing},"none")
        , (Name {nameLocalName = "stroke", nameNamespace = Nothing, namePrefix = Nothing},"red")
        , (Name {nameLocalName = "d", nameNamespace = Nothing, namePrefix = Nothing}
            ,"M10,30 A20,20 0,0,1 50,30 A20,20 0,0,1 90,30 Q90,60 50,90 Q10,60 10,30 z")
        ]
      , elementNodes = []
      }
    ]
  }


-- heart with shade
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
-- <svg viewBox="-40 0 150 100">
--  <g fill="grey"
--     transform="rotate(-10 50 100)
--                translate(-36 45.5)
--                skewX(40)
--                scale(1 0.5)">
--    <path id="heart" d="M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 z" />
--  </g>

--  <use href="#heart" fill="none" stroke="red"/>
-- </svg>
heartWithShade = Element {
    elementName = simpleName "svg"
    , elementAttributes = Map.fromList [
       (Name { nameLocalName = "viewBox" , nameNamespace = Nothing , namePrefix = Nothing }
       , "-40 0 150 100")
    ]
    , elementNodes = [
        NodeElement (Element {
          elementName = simpleName "g"
        , elementAttributes = Map.fromList [
                  (simpleName "fill", "grey")
                , (simpleName "transform",
                   "rotate(-10 50 100) translate(-36 45.5) skewX(40) scale(1 0.5)")
                        ]
                  , elementNodes =
                        [ NodeElement
                            ( Element
                                { elementName = simpleName "path"
                                , elementAttributes = Map.fromList
                                    [
                                        ( simpleName "d"
                                        , "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 z"
                                        )
                                    ,
                                        (simpleName "id", "heart")
                                    ]
                                , elementNodes = []
                                }
                            )
                        ]
                    }
                )
        , NodeElement ( Element {
             elementName = simpleName "use"
           , elementAttributes = Map.fromList [
                          (simpleName "fill", "none")
                        , (simpleName "href", "#heart")
                        , (simpleName "stroke", "red")
                        ]
                    , elementNodes = []
                    }
                )
            ]
        }




