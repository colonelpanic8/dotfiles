#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import System.Process
import System.Exit
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (forM, when)
import Data.Time.Clock
import Data.Time.Format
import System.IO

-- Types
data Device = Device { deviceName :: String, deviceBrightness :: Int }
    deriving (Show)

data BrightnessCommand = Absolute Int | Increase Int | Decrease Int | Query
    deriving (Show)

-- Logging
logAction :: String -> IO ()
logAction arg = do
    time <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    ppid <- readProcess "sh" ["-c", "ps -p $PPID -o comm= 2>/dev/null || echo unknown"] ""
    pwd <- readProcess "pwd" [] ""
    let logEntry = "[" ++ timeStr ++ "] Called with: '" ++ arg ++ "' | Parent: " ++
                   filter (/= '\n') ppid ++ " | PWD: " ++ filter (/= '\n') pwd ++ "\n"
    appendFile "/tmp/brightness.log" logEntry

-- Parse command line argument
parseArg :: String -> BrightnessCommand
parseArg "" = Query
parseArg ('+':rest) = case readMaybe rest of
    Just n -> Increase n
    Nothing -> Query
parseArg ('-':rest) = case readMaybe rest of
    Just n -> Decrease n
    Nothing -> Query
parseArg s = case readMaybe s of
    Just n -> Absolute n
    Nothing -> Query

-- Get list of backlight devices
getBacklightDevices :: IO [String]
getBacklightDevices = do
    output <- readProcess "brightnessctl" ["--list"] ""
    return $ mapMaybe extractDevice (lines output)
  where
    extractDevice line
        | "Device '" `isInfixOf` line && "class 'backlight'" `isInfixOf` line =
            let afterDevice = drop (length "Device '") line
                deviceName = takeWhile (/= '\'') afterDevice
            in if null deviceName then Nothing else Just deviceName
        | otherwise = Nothing

-- Get brightness percentage for a device
getDeviceBrightness :: String -> IO (Maybe Int)
getDeviceBrightness device = do
    output <- readProcess "brightnessctl" ["-d", device] ""
    return $ extractPercentage output
  where
    extractPercentage s = case dropWhile (/= '(') s of
        ('(':rest) -> readMaybe (takeWhile (/= '%') rest)
        _ -> Nothing

-- Set brightness for a device
setDeviceBrightness :: String -> String -> IO ()
setDeviceBrightness device cmd = do
    _ <- readProcess "brightnessctl" ["-d", device, "set", cmd] ""
    return ()

-- Build brightness command string
buildCommand :: BrightnessCommand -> String
buildCommand (Absolute n) = show n ++ "%"
buildCommand (Increase n) = show n ++ "%+"
buildCommand (Decrease n) = show n ++ "%-"
buildCommand Query = ""

-- Apply brightness change to all devices
applyBrightness :: BrightnessCommand -> IO ()
applyBrightness Query = return ()
applyBrightness cmd = do
    devices <- getBacklightDevices
    let cmdStr = buildCommand cmd
    
    -- Log what we're about to do
    appendFile "/tmp/brightness.log" $ 
        "  Applying: " ++ cmdStr ++ " to devices: " ++ show devices ++ "\n"

    -- Apply to all devices
    mapM_ (\dev -> setDeviceBrightness dev cmdStr) devices

    -- Sync devices if needed
    when (length devices > 1) $ do
        brightnesses <- forM devices $ \dev -> do
            mbBright <- getDeviceBrightness dev
            return (dev, mbBright)

        let validBrightnesses = [(d, b) | (d, Just b) <- brightnesses]
        when (not $ null validBrightnesses) $ do
            let values = map snd validBrightnesses
                maxBright = maximum values
                minBright = minimum values

            -- If devices are out of sync
            when (maxBright /= minBright) $ do
                -- Use minimum when brightness is low (15% or below)
                let syncValue = if maxBright <= 15 then minBright else maxBright
                -- Log sync decision for debugging
                appendFile "/tmp/brightness.log" $
                    "  Syncing: max=" ++ show maxBright ++ "%, min=" ++ show minBright ++
                    "%, using=" ++ show syncValue ++ "%\n"
                mapM_ (\dev -> setDeviceBrightness dev (show syncValue ++ "%")) devices

-- Get average brightness across all devices
getAverageBrightness :: IO Int
getAverageBrightness = do
    devices <- getBacklightDevices
    if null devices
        then return 50  -- Default fallback
        else do
            brightnesses <- forM devices getDeviceBrightness
            let validValues = [b | Just b <- brightnesses]
            if null validValues
                then return 50
                else return $ sum validValues `div` length validValues

-- Send notification using rumno if available
sendNotification :: Int -> IO ()
sendNotification brightness = do
    let timeoutSeconds = "2.5"
    rumnoExists <- (== ExitSuccess) <$>
                   rawSystem "sh" ["-c", "command -v rumno >/dev/null 2>&1"]
    if rumnoExists
        then do
            _ <- readProcess "rumno" ["notify", "-t", timeoutSeconds, "-b", show brightness] ""
            return ()
        else putStrLn (show brightness)

-- Check if a string contains another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

main :: IO ()
main = do
    args <- getArgs
    let arg = case args of
                [] -> ""
                (x:_) -> x

    -- Log the invocation
    logAction arg

    -- Parse and apply command
    let cmd = parseArg arg
    applyBrightness cmd

    -- Get current brightness and notify
    brightness <- getAverageBrightness
    sendNotification brightness
