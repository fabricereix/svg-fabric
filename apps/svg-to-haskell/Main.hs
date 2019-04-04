{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.Aeson
import GHC.Generics

import qualified Data.Char as Char
import Data.String.Conversions
import Text.ParserCombinators.Parsec
import Data.List
import Svg.Spec.Core


capitalized :: String -> String
capitalized (x:xs) = Char.toUpper x : map Char.toLower xs
capitalized [] = []


 


-- Haskell Source --


data HaskellModule = HaskellModule {
    name :: String
  , elements :: [HaskellElement]
  , attributes :: [HaskellAttribute]
  , attributes2 :: [String]
  } deriving (Show, Generic)
data HaskellElement = HaskellElement {
    name :: String
  , type_name :: String
  , attributes :: [HaskellAttribute]
  , first :: Bool
  } deriving (Show, Generic)

data HaskellAttribute = HaskellAttribute {
    _name :: String
  , _field :: String
  , _type :: String
  , _comma :: Bool
  , _default :: String
  , _optional :: Bool
  } deriving (Show, Generic)

instance ToJSON HaskellElement
instance ToJSON HaskellModule
instance ToJSON HaskellAttribute where
 toJSON HaskellAttribute{..} = object [
    "name" .= _name
  , "field" .= _field
  , "type"  .= _type
  , "comma" .= _comma
  , "default" .= _default
  , "optional" .= _optional
  ]




generateHaskellElement :: (Int, Element) -> [Attribute] -> HaskellElement
generateHaskellElement (index, Element {
    _name = name
  , _attributes = attributes
  }) presAttributes = HaskellElement {
                        name=name
                      , first = index == 0
                      , type_name=capitalized name
                      , attributes=map generateHaskellAttribute $ zip [0..] $ attributes ++ presAttributes
       }

generateHaskellAttribute :: (Int, Attribute) -> HaskellAttribute
generateHaskellAttribute (index, Attribute {_name=name, _type=types}) = 
  HaskellAttribute {
    _name = name
  , _field ="_" ++ name
  , _type=haskellAttributeTypes types
  , _comma=index > 0
  , _default="OneOf" ++ show (length types) ++ " def"
  , _optional = (head types == NONE_TYPE)
  }

haskellAttributeType :: AttributeType -> String
haskellAttributeType LENGTH = "Length"
haskellAttributeType NONE_TYPE = "None"
haskellAttributeType PERCENTAGE = "Percentage"
haskellAttributeType AUTO_TYPE = "Auto"
haskellAttributeType OPACITY = "Opacity"
haskellAttributeType PAINT = "Paint"
haskellAttributeType TRANSFORM_LIST = "[Transform]"
haskellAttributeType POINTS = "[Point]"
haskellAttributeType VIEWBOX = "ViewBox"

haskellAttributeTypes :: [AttributeType] -> String
haskellAttributeTypes types = 
                "OneOf" ++ show (length types') ++ " "
                ++ (intercalate " " $ map haskellAttributeType types')

                where types' = if (head types) == NONE_TYPE then tail types else types




haskellAttributeDefault :: [AttributeType] -> AttributeDefault -> String
haskellAttributeDefault types AUTO = "OneOf" ++ show (length types) ++ " AUTO"
haskellAttributeDefault types@(LENGTH:_) ZERO = "OneOf" ++ show (length types) ++ " (Length 0)" 
haskellAttributeDefault types@(POINTS:_) NONE = "OneOf" ++ show (length types) ++ " []" 
haskellAttributeDefault types d = error $ "invalid combination " ++ show types ++ " " ++ show d


-- parse text spec

attributesFile :: Parser [Attribute]
attributesFile = do
   attrs <- many1 $ do attr <- attribute
                       skipMany1 (char '\n')
                       return attr
   eof
   return attrs

elementsFile :: Parser [Element]
elementsFile = do
   elems <- many1 $ do e <- element
                       skipMany1 (char '\n')
                       return e
   eof
   return elems

attribute :: Parser Attribute
attribute = do
    _name <- many1 letter
    skipMany1 (char ' ')
    _type <- attributeType `sepBy1` (char '|')
    skipMany (char ' ')
    _default <- attributeDefault
    return Attribute{..}


attributeType :: Parser AttributeType
attributeType = try (do
    s <- many1 (alphaNum <|> oneOf "[]<>-")
    case s of
       "auto"             -> return AUTO_TYPE
       "none"             -> return NONE_TYPE
       "<length>"         -> return LENGTH
       "<percentage>"     -> return PERCENTAGE
       "<opacity-value>"  -> return OPACITY
       "<paint>"          -> return PAINT
       "<transform-list>" -> return TRANSFORM_LIST
       "[points]"         -> return POINTS
       "[viewbox]"        -> return VIEWBOX
       _                  -> fail "invalid attribute type"
      ) <?> "attributeType"

attributeDefault :: Parser AttributeDefault
attributeDefault = try (do
    s <- many alphaNum
    case s of
       "auto"  -> return AUTO
       "0"     -> return ZERO
       "black" -> return BLACK
       ""      -> return NONE
       _       -> fail "Invalid default attribute"
      ) <?> "attributeDefault"


element :: Parser Element
element = do _name <- many1 letter
             _attributes <- many $ try $ do 
                 skipMany1 (char '\n')
                 skipMany (char ' ')
                 attribute
             return Element{..}


main :: IO()
main = do
  let presAttributeFilename =  "spec/presentation-attributes.spec"
  let elemsFilename =  "spec/elements.spec"

  presAttributes <- readFile presAttributeFilename
  case parse attributesFile (cs presAttributeFilename) presAttributes of
    Left e -> print e
    Right attrs -> do
      -- print attrs
      elems <- readFile elemsFilename
      -- putStrLn elems
      case parse elementsFile (cs elemsFilename) elems of
        Left e -> print e
        Right elements -> do
           -- print elements
           -- print$ map (\Attribute{_name=name}->name) $ concatMap (\Element{_attributes=attributes}->attributes) elements
           putStrLn $ cs $ encode $ HaskellModule {
             name="Svg.Elements"                   
           , elements=map (\(i,e)->generateHaskellElement (i,e) attrs) $ zip [0..] elements
           , attributes = map generateHaskellAttribute $ zip [0..] attrs
           , attributes2=map (\Attribute{_name=name}->name) $ concatMap (\Element{_attributes=attributes}->attributes) elements
           }  
 
-- -- test utils
parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> manyTill anyToken eof) ""
