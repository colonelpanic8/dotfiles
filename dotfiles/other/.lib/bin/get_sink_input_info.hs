#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Data.Text (unpack)
import System.Process
import Text.Printf
import Text.Regex.Posix
import System.Exit

main :: IO ()
main = do
  out <- getSinkText
  let sinkTexts = splitOn "\nSink " out
      getMatches txt regex = getAllTextMatches $ txt =~ regex :: [String]
      getPair regex txt = frth $ (txt =~ regex :: (String, String, String, [String]))
      frth (_,_,_,a:b:_) = (map dotToUnderscore a, b)
      dotToUnderscore '.' = '_'
      dotToUnderscore c = c
      getPairs txt regex = map (getPair regex) $ getMatches txt regex
      getSinkMap' txt = M.union (M.fromList $ getPairs txt propertyRegex)
                       (M.fromList $ getPairs txt fieldRegex)
      getSinkMap txt = M.insert "sink_input_id" (getSinkNumber txt) $ getSinkMap' txt
      getSinkNumber txt = case txt =~ "Input #([0-9]*)" :: (String, String, String, [String]) of
                            (_,_,_,a) -> head a
  mapM_ (T.putStrLn . T.decodeUtf8 . encode . getSinkMap) sinkTexts
    where getSinkText = do
                  (_, txt, _) <- readCreateProcessWithExitCode (shell "pactl list sink-inputs") ""
                  return txt
          propertyRegex = "^[\t\n ]+([^\n:]*) = \"([^\n]*)\""
          fieldRegex = "^[\t\n ]+(.*?): ([^\n ]*)"
