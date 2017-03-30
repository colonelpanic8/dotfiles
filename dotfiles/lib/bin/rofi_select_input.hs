#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (unpack)
import Text.Regex
import Text.Printf
import Turtle hiding (printf, find)

main :: IO ()
main = do
  out <- unpack <$> getSinkText
  let sinkInfos = splitOn "\nSink" out
      matches = catMaybes $ matchRegex sinkRegex <$> sinkInfos
      entries = map buildEntry matches
  (exitCode, selection) <- shellStrict "rofi -dmenu -i -kb-custom-1 'Alt-o'"
                           (select $ map fromString entries)
  let selectedSink = head $ splitOn " " $ unpack selection
      unMuteSelected = setMuteAction "0" selectedSink
      selectedIsMuted = fromMaybe True $
                        isMuted . (!! 1) <$> find ((== selectedSink) . head) matches
  print selectedSink
  print matches
  print selectedIsMuted
  case exitCode of
    ExitSuccess ->
      void $ setMuteAction (toSetString selectedIsMuted) selectedSink
    ExitFailure 10 ->
      do
        mapM_ (setMuteAction "1" . head) matches
        void unMuteSelected
    ExitFailure _ -> return ()
    where getSinkText = snd <$> shellStrict "pactl list sink-inputs" empty
          sinkRegex = mkRegexWithOpts "Input .([0-9]*).*?Mute: ([^\n]*).*?application.name =([^\n]*)" False True
          buildEntry (num:status:name:_) =
            printf "%s - %s%s" num (trim $ noQuotes name) (muteString status)
          buildEntry _ = ""
          setMuteAction status sink = shell (fromString $ setMuteCommand status sink) empty
          setMuteCommand status sink = "pactl set-sink-input-mute " ++ sink ++ " " ++ status
          trim = dropWhileEnd (== ' ') . dropWhile (== ' ')
          isMuted = (== "yes")
          muteString status = if isMuted status then  " (Muted)" else "" :: String
          noQuotes = filter (not . (`elem` ("\"" :: String)))
          toSetString True = "0"
          toSetString False = "1"
