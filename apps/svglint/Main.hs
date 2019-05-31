{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase   #-}
module Main where

{-
TODO: make sure that your transforms are commutatives

-}
-- import qualified Data.ByteString.Char8         as B
import           Data.String.Conversions
---import           Data.Text.Encoding
import           Data.Time
import           Data.Version                  (showVersion)
import           Language.Haskell.TH           (runIO, stringE)
import           Paths_svg_fabric              (version)
import           System.Console.GetOpt
import           System.Environment            (getArgs)
import           System.Exit
import Text.XML hiding (readFile)
import qualified Data.Map as Map
import qualified Data.Text as T

-- import qualified Svg.Setter.Svg as SvgSetter
-- import Svg.Types.Core
import Svg.Types.Format
import Svg.Types.Parser

-- import Prelude hiding (id, length) as Prelude
import qualified Prelude as P
import Prelude hiding (id, length)



data Options = Options  {
    optShowVersion      :: Bool
  , optPretty           :: Bool
  , optNormalizeValues  :: Bool
  , optRemoveUnknown    :: Bool
  , optRemoveDefault    :: Bool
  , optOptimizePaths    :: Bool
  , optHelp             :: Bool
  } deriving (Show, Eq)


defaultOptions :: Options
defaultOptions = Options {
    optShowVersion      = False
  , optPretty           = False
  , optNormalizeValues  = False
  , optRemoveUnknown    = False
  , optRemoveDefault    = False
  , optOptimizePaths    = False
  , optHelp             = False
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v']  ["version"]
      (NoArg (\ opts -> opts { optShowVersion = True }))
      "show version"
  , Option ['h']  ["help"]
      (NoArg (\ opts -> opts { optHelp = True }))
      "print usage"
  , Option []     ["pretty"]
      (NoArg (\ opts -> opts { optPretty = True }))
      "Indent XML"
  , Option []     ["normalize-values"]
      (NoArg (\ opts -> opts { optNormalizeValues = True }))
      "normalize attribute values"
  , Option []     ["remove-unknown"]
      (NoArg (\ opts -> opts { optRemoveUnknown = True }))
      "remove non-svg attributes"
  , Option []     ["remove-default"]
      (NoArg (\ opts -> opts { optRemoveDefault = True }))
      "remove attributes with default value"
  , Option ['c']  ["optimize-paths"]
      (NoArg (\ opts -> opts { optOptimizePaths = True }))
      "Optimize Paths"
  ]


svgLint :: [Element->Element] -> Document -> Document
--svgLint normalizeValue removeAttributes optimizePath d = d
svgLint transforms doc@Document {documentRoot=root}
  = doc{documentRoot=foldl (\e t-> t e) root transforms}


main :: IO()
main = do
  args <- getArgs
  case getOpt Permute options args of
         (o, inputFiles, []) -> do
             let opts = foldl (flip P.id) defaultOptions o
                 transforms =
                     if optRemoveDefault opts then [removeDefault] else []
                  ++ if optRemoveUnknown opts then [removeUnknown] else []
                  ++ if optNormalizeValues opts then [normalizeValue] else []
                  ++ [optimizePaths | optOptimizePaths opts]
             if optShowVersion opts then printVersion
             else
               if null inputFiles || optHelp opts then printUsage
               else do
                 s <- getFileContent (head inputFiles)
                 case parseLBS def (cs s) of
                   Left e -> print e
                   Right doc -> putStrLn $ cs $ renderText def{rsPretty=optPretty opts} $ svgLint transforms doc

         (_, _, errors) -> print errors



removeUnknown :: Element -> Element
removeUnknown element@Element {
    elementName=elementName
  , elementAttributes=attributes
  , elementNodes=children
  } = element {
     elementName=elementName
   , elementAttributes=Map.fromList $ filter (attributeExist elementName) $ Map.toList  attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ removeUnknown element2
                             x -> x) children
   }

removeDefault :: Element -> Element
removeDefault element@Element {
    elementName=elementName
  , elementAttributes=attributes
  , elementNodes=children
  } = element {
     elementName=elementName
   , elementAttributes=Map.fromList $ filter (defaultAttribute' elementName) $ Map.toList  attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ removeDefault element2
                             x -> x) children
   }


normalizeValue :: Element -> Element
normalizeValue element@Element {
    elementName=elementName
  , elementAttributes=attributes
  , elementNodes=children
  } = element {
     elementName=elementName
   , elementAttributes=Map.fromList $ map (normalizeValue' elementName) $ Map.toList attributes
   , elementNodes=map (\case NodeElement element2 -> NodeElement $ removeDefault element2
                             x -> x) children
   }


attributeExist :: Name -> (Name, T.Text) -> Bool
attributeExist Name{nameLocalName=elementName} (Name{nameLocalName=attributeName},_) = exists (cs elementName) (cs attributeName)

defaultAttribute' :: Name -> (Name, T.Text) -> Bool
defaultAttribute' Name{nameLocalName=elementName} (Name{nameLocalName=attributeName},value) = not $ defaultAttribute (cs elementName) (cs attributeName) (cs value)

normalizeValue' :: Name -> (Name, T.Text) -> (Name, T.Text)
normalizeValue' Name{nameLocalName=elementName} (name@Name{nameLocalName=attributeName}, attributeValue) = case normalizeAttributeValue (cs elementName) (cs  attributeName) (cs attributeValue) of
  Left e -> error e
  Right value -> (name, cs value)


exists :: String -> String -> Bool
exists _ "unknown" = error "got unknown!!"
exists "svg" "viewBox" = True
exists "circle" attributeName = circleExists attributeName
exists "path" attributeName = pathExists attributeName
exists _ _ = False

circleExists :: String -> Bool
circleExists "cx" = True
circleExists "cy" = True
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
svgNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist"


circleNormalizeValue :: String -> String -> Either String String
circleNormalizeValue "cx" s = case length s of
         Left e -> Left e
         Right v -> Right $ formatLength v
circleNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist"

pathNormalizeValue :: String -> String -> Either String String
pathNormalizeValue "d" s = case path s of
         Left e -> Left e
         Right v -> Right $ formatPath v
pathNormalizeValue name _ = error $ "attribute " ++ name ++ " does not exist"


normalizeAttributeValue :: String -> String -> String -> Either String String
normalizeAttributeValue "svg" name value = svgNormalizeValue name value
normalizeAttributeValue "circle" name value = circleNormalizeValue name value
normalizeAttributeValue "path" name value = pathNormalizeValue name value
normalizeAttributeValue name _ _ = error $ "invalid element " ++ name




optimizePaths :: Element -> Element
optimizePaths element@Element{elementAttributes=attributes}
  = element{elementAttributes=Map.fromList $ Map.toList attributes ++[newAttrib]}
   where newAttrib = (Name {nameLocalName="x", nameNamespace=Nothing, namePrefix=Nothing}, "x")



getFileContent :: String -> IO String
getFileContent "-"      = getContents
getFileContent filename = readFile filename


printUsage :: IO()
printUsage = putStrLn $ usageInfo "usage: svglint [OPTIONS] FILE" options


printVersion :: IO()
printVersion = do
    putStrLn $ "svglint" ++ " " ++ getVersion
    putStrLn $ "Build at " ++ $(stringE =<< runIO (show `fmap` Data.Time.getCurrentTime))
    exitSuccess


getVersion :: String
getVersion = case showVersion Paths_svg_fabric.version of
                    "0.0.0" -> "snapshot"
                    v       -> v


