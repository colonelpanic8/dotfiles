#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (unpack)
import Text.Regex
import Text.Printf
import Turtle hiding (printf)
import Turtle.Shell

main = do
  out <- unpack <$> getSinkText
  let sinkInfos = splitOn "\nSink" out
      matches = catMaybes $ matchRegex sinkRegex <$> sinkInfos
      entries = map buildEntry matches
      rofiText = intercalate "\n" entries
  selection <- snd <$> shellStrict "rofi -dmenu -i" (select $ map fromString entries)
  let selectedSink = head $ splitOn " " $ unpack selection
  mapM_ (setMuteAction "1" . head) matches
  setMuteAction "0" selectedSink
  return ()
    where getSinkText = snd <$> shellStrict "pactl list sink-inputs" empty
          sinkRegex = mkRegexWithOpts "Input .([0-9]*).*?application.name =([^\n]*)" False True
          buildEntry (num:name:_) = let app = (filter (not . (`elem` ("\"" :: String))) name) in printf "%s - %s" num $ trim app
          setMuteAction status sink = shell (fromString $ setMuteCommand status sink) empty
          setMuteCommand status sink = "pactl set-sink-input-mute " ++ sink ++ " " ++ status
          trim = dropWhileEnd (== ' ') . dropWhile (== ' ')
