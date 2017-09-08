#!/usr/bin/env run_haskell_stack.sh
{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (unpack)
import System.Process
import Text.Printf
import Text.Regex
import System.Exit

main :: IO ()
main = do
  out <- getSinkText
  let sinkInfos = splitOn "\nSink" out
      matches = catMaybes $ matchRegex sinkRegex <$> sinkInfos
      entries = map buildEntry matches
  (exitCode, selection, _) <- readCreateProcessWithExitCode (shell "rofi -dmenu -i -kb-custom-1 'Alt-o'") $
                           intercalate "\n" entries
  let selectedSink = head $ splitOn " " selection
      unMuteSelected = setMuteAction "0" selectedSink
      selectedIsMuted = fromMaybe True $
                        isMuted . (!! 1) <$> find ((== selectedSink) . head) matches
      setAll state = mapM_ (setMuteAction state . head) matches
  print selectedSink
  print matches
  print selectedIsMuted
  case exitCode of
    ExitSuccess ->
      void $ setMuteAction (toSetString selectedIsMuted) selectedSink
    ExitFailure 10 ->
      do
        setAll "1"
        void unMuteSelected
    ExitFailure _ -> setAll "0"
    where getSinkText = do
                  (_, txt, _) <- readCreateProcessWithExitCode (shell "pactl list sink-inputs") ""
                  return txt
          sinkRegex = mkRegexWithOpts "Input .([0-9]*).*?Mute: ([^\n]*).*?application.name =([^\n]*)" False True
          buildEntry (num:status:name:_) =
            printf "%s - %s%s" num (trim $ noQuotes name) (muteString status)
          buildEntry _ = ""
          setMuteAction status sink = callCommand $ setMuteCommand status sink
          setMuteCommand status sink = "pactl set-sink-input-mute " ++ sink ++ " " ++ status
          trim = dropWhileEnd (== ' ') . dropWhile (== ' ')
          isMuted = (== "yes")
          muteString status = if isMuted status then  " (Muted)" else "" :: String
          noQuotes = filter (not . (`elem` ("\"" :: String)))
          toSetString True = "0"
          toSetString False = "1"
