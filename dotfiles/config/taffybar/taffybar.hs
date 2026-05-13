module Main (main) where

import Network.HostName (getHostName)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Log.Logger (Priority (WARNING), rootLoggerName, setLevel, updateGlobalLogger)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (appendHook, detectBackend)
import System.Taffybar.DBus
import System.Taffybar.DBus.Toggle
import System.Taffybar.Hooks (withLogLevels)
import System.Taffybar.Information.ChromeWindowInfo (registerChromeWindowInfoRefreshRequests)
import System.Taffybar.SimpleConfig (toTaffybarConfig)
import TaffybarConfig.Config (mkSimpleTaffyConfig)
import TaffybarConfig.Host (cssFilesForHost)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel WARNING)

  hostName <- getHostName
  backend <- detectBackend
  cssFiles <- mapM (getUserConfigFile "taffybar") (cssFilesForHost hostName)

  let simpleTaffyConfig = mkSimpleTaffyConfig hostName backend cssFiles
  startTaffybar $
    withLogServer $
      withLogLevels $
        withToggleServer $
          appendHook registerChromeWindowInfoRefreshRequests $
            toTaffybarConfig simpleTaffyConfig
