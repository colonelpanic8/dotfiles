module Main (main) where

import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Log.Logger (Priority (WARNING), rootLoggerName, setLevel, updateGlobalLogger)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (appendHook, detectBackend)
import System.Taffybar.DBus
import System.Taffybar.DBus.Toggle
import System.Taffybar.Hooks (withLogLevels)
import System.Taffybar.Information.ChromeWindowInfo (registerChromeWindowInfoRefreshRequests)
import TaffybarConfig.Config (mkTaffybarConfig)
import TaffybarConfig.RuntimeStats (startRuntimeStatsLogging)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  startRuntimeStatsLogging

  backend <- detectBackend
  cssFiles <- mapM (getUserConfigFile "taffybar") ["adaptive.css"]

  let taffybarConfig = mkTaffybarConfig backend cssFiles
  startTaffybar $
    withLogServer $
      withToggleServer $
        appendHook registerChromeWindowInfoRefreshRequests $
          withLogLevels $
            taffybarConfig
