{-# LANGUAGE NumericUnderscores #-}

module TaffybarConfig.RuntimeStats
  ( startRuntimeStatsLogging,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import GHC.Stats
  ( GCDetails (..),
    RTSStats (..),
    getRTSStats,
    getRTSStatsEnabled,
  )
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

runtimeStatsLogPath :: String
runtimeStatsLogPath = "TaffybarConfig.RuntimeStats"

runtimeStatsIntervalMicros :: Int
runtimeStatsIntervalMicros = 5 * 60 * 1_000_000

-- | Log a compact RTS heap/GC snapshot after startup and every five minutes.
-- This is disabled automatically unless the executable was started with
-- @+RTS -T@.
startRuntimeStatsLogging :: IO ()
startRuntimeStatsLogging = do
  enabled <- getRTSStatsEnabled
  void $
    forkIO $ do
      threadDelay 30_000_000
      hPutStrLn stderr $
        runtimeStatsLogPath <> ": logging " <> if enabled then "enabled" else "disabled (start with +RTS -T)"
      when enabled $
        forever $ do
          getRTSStats >>= logRuntimeStats
          threadDelay runtimeStatsIntervalMicros

logRuntimeStats :: RTSStats -> IO ()
logRuntimeStats stats =
  hPutStrLn stderr $
    printf
      "%s: live=%.1fMiB max_live=%.1fMiB max_mem=%.1fMiB allocated=%.1fMiB copied=%.1fMiB gcs=%d major_gcs=%d mutator_cpu=%.1fs gc_cpu=%.1fs elapsed=%.1fs"
      runtimeStatsLogPath
      (toMiB $ gcdetails_live_bytes $ gc stats)
      (toMiB $ max_live_bytes stats)
      (toMiB $ max_mem_in_use_bytes stats)
      (toMiB $ allocated_bytes stats)
      (toMiB $ copied_bytes stats)
      (gcs stats)
      (major_gcs stats)
      (toSeconds $ mutator_cpu_ns stats)
      (toSeconds $ gc_cpu_ns stats)
      (toSeconds $ elapsed_ns stats)

toMiB :: (Integral a) => a -> Double
toMiB bytes = fromIntegral bytes / (1024 * 1024)

toSeconds :: (Integral a) => a -> Double
toSeconds nanoseconds = fromIntegral nanoseconds / 1_000_000_000
