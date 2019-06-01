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
-- import qualified Data.Map as Map
-- import qualified Data.Text as T

-- import qualified Svg.Setter.Svg as SvgSetter
-- import Svg.Types.Core
import Svg.ElementTransforms

-- import Prelude hiding (id, length) as Prelude
import qualified Prelude as P
import Prelude hiding (id, length)



data Options = Options  {
    optShowVersion      :: Bool
  , optPretty           :: Bool
  , optRemoveEmpty      :: Bool
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
  , optRemoveEmpty      = False
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
  , Option []     ["remove-empty"]
      (NoArg (\ opts -> opts { optRemoveEmpty = True }))
      "remove empty attributes"
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
                     [removeDefault  | optRemoveDefault opts]
                  ++ [removeUnknown  | optRemoveUnknown opts]
                  ++ [removeEmpty    | optRemoveEmpty opts]
                  ++ [normalizeValue | optNormalizeValues opts]
                  ++ [optimizePaths  | optOptimizePaths opts]
             if optShowVersion opts then printVersion
             else
               if null inputFiles || optHelp opts then printUsage
               else do
                 s <- getFileContent (head inputFiles)
                 case parseLBS def (cs s) of
                   Left e -> print e
                   Right doc -> putStrLn $ cs $ renderText def{rsPretty=optPretty opts} $ svgLint transforms doc

         (_, _, errors) -> print errors

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

