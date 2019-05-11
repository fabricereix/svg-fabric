--{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import System.IO.Unsafe
-- import Text.Pretty.Simple
-- import Control.Monad.Catch
-- import Data.Either
-- import Data.Map
-- import qualified Data.Text as T
import Data.String.Conversions
import Text.XML
import Text.Pretty.Simple (pPrint)


main :: IO()
main = do
   s <- getContents
   case parseLBS def (cs s) of
     Left e -> print e
     Right doc -> pPrint doc

