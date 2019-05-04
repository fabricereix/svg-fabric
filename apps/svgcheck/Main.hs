{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import System.IO.Unsafe
-- import Text.Pretty.Simple
-- import Control.Monad.Catch
-- import Data.Either
-- import Data.Map
-- import qualified Data.Text as T
-- import Data.String.Conversions
import System.Environment(getArgs)
import System.Console.GetOpt
import Text.XML hiding (readFile)
import Data.String.Conversions
import Svg.Validator
import Svg.Validator.Core


data Flag = Version | Input String deriving (Show)
data Argument = String


check :: Document -> IO()
check Document {documentRoot=root} = do
   let errors = validate root
   mapM_ (putStrLn . formatError) errors

formatError :: Error -> String
formatError (InvalidAttribute element (Name {nameLocalName=attribute})) = "Invalid Attribute \"" ++ (cs attribute) ++ "\" for element \"" ++ element ++ "\""
formatError (InvalidAttributeValue element attribute s) = "attribute value \"" ++ (cs s) ++ "\" is not valid for attribute " ++ attribute ++ " for element " ++ element
formatError e = error $ show e


run :: [String] -> [Flag] -> IO ()
run args _ = do
   -- putStrLn $ "arguments=" ++ unwords args
   --putStrLn $ "options=" ++ show opts
   let filename = head args
   s <- readFile filename
   case parseLBS def (cs s) of
     Left e -> print e
     Right doc -> check doc


main :: IO()
main = do
   args <- getArgs
   case getOpt Permute options args of
     (flags, args2, [])   -> run args2 flags
     (_,     _,    _) -> error $ "error usage"


options :: [OptDescr Flag]
options = [
    Option ['v'] ["version"] (NoArg Version) "show version number"
  ]

