{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Network.HostName
import           StatusNotifier.Tray
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Process
import           System.Taffybar
import           System.Taffybar.Auth
import           System.Taffybar.Context (appendHook)
import           System.Taffybar.DBus
import           System.Taffybar.DBus.Toggle
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.Memory
import           System.Taffybar.Information.X11DesktopInfo
import           System.Environment.XDG.BaseDir
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces
import           Text.Printf
import           Text.Read hiding (lift)

mkRGBA (r, g, b, a) = (r/256, g/256, b/256, a/256)
blue = mkRGBA (42, 99, 140, 256)
yellow1 = mkRGBA (242, 163, 54, 256)
yellow2 = mkRGBA (254, 204, 83, 256)
yellow3 = mkRGBA (227, 134, 18, 256)
red = mkRGBA (210, 77, 37, 256)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [(0.129, 0.588, 0.953, 1)]
  , graphLabel = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
  , graphLabel = Just "cpu"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

getFullWorkspaceNames :: X11Property [(WorkspaceId, String)]
getFullWorkspaceNames = go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where go = zip [WorkspaceId i | i <- [0..]]

workspaceNamesLabelSetter workspace =
  fromMaybe "" . lookup (workspaceIdx workspace) <$>
            liftX11Def [] getFullWorkspaceNames

enableLogger logger level = do
  logger <- getLogger logger
  saveGlobalLogger $ setLevel level logger

logDebug = do
  global <- getLogger ""
  saveGlobalLogger $ setLevel DEBUG global
  logger3 <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel DEBUG logger3
  logger <- getLogger "System.Taffybar.Widget.Generic.AutoSizeImage"
  saveGlobalLogger $ setLevel DEBUG logger
  logger2 <- getLogger "StatusNotifier.Tray"
  saveGlobalLogger $ setLevel DEBUG logger2
  -- workspacesLogger <- getLogger "System.Taffybar.Widget.Workspaces"
  -- saveGlobalLogger $ setLevel WARNING workspacesLogger
  -- logDebug
  -- logM "What" WARNING "Why"
  -- enableLogger "System.Taffybar.Widget.Util" DEBUG
  -- enableLogger "System.Taffybar.Information.XDG.DesktopEntry" DEBUG
  -- enableLogger "System.Taffybar.WindowIcon" DEBUG
  -- enableLogger "System.Taffybar.Widget.Generic.PollingLabel" DEBUG

cssFileByHostname =
  [ ("uber-loaner", "uber-loaner.css")
  , ("imalison-home", "taffybar.css")
  , ("ivanm-dfinity-razer", "ivanm-dfinity-razer.css")
  , ("ryzen-shine", "taffybar.css")
  ]

main = do
  hostName <- getHostName
  homeDirectory <- getHomeDirectory
  cssFilePath <-
    traverse (getUserConfigFile "taffybar") $ lookup hostName cssFileByHostname
  let cpuGraph = pollingGraphNew cpuCfg 5 cpuCallback
      memoryGraph = pollingGraphNew memCfg 5 memCallback
      myIcons = scaledWindowIconPixbufGetter $
                getWindowIconPixbufFromChrome <|||>
                unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> lift $ loadPixbufByName size "application-default-icon")
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]
      myWorkspacesConfig =
        defaultWorkspacesConfig
        { underlineHeight = 3
        , underlinePadding = 2
        , minIcons = 1
        , getWindowIconPixbuf = myIcons
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter = workspaceNamesLabelSetter
        }
      workspaces = workspacesNew myWorkspacesConfig
      myClock =
        textClockNewWith
        defaultClockConfig
        { clockUpdateStrategy = RoundedTargetInterval 60 0.0
        , clockFormatString = "%a %b %_d %I:%M %p"
        }
      fullEndWidgets =
        map (>>= buildContentsBox)
              [ myClock
              , sniTrayNew
              , cpuGraph
              , memoryGraph
              , networkGraphNew netCfg Nothing
              -- , networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 200
              -- , fsMonitorNew 60 ["/dev/sdd2"]
              , mpris2New
              ]
      shortLaptopEndWidgets =
        map (>>= buildContentsBox)
                       [ batteryIconNew
                       , textBatteryNew "$percentage$%"
                       , myClock
                       , sniTrayNew
                       , mpris2New
                       ]
      longLaptopEndWidgets =
        map (>>= buildContentsBox)
              [ batteryIconNew
              , textBatteryNew "$percentage$%"
              , textClockNewWith defaultClockConfig
              , sniTrayNew
              , cpuGraph
              , memoryGraph
              , networkGraphNew netCfg Nothing
              , mpris2New
              ]
      baseConfig =
        defaultSimpleTaffyConfig
        { startWidgets =
             workspaces : map (>>= buildContentsBox) [layout, windows]
        , endWidgets = fullEndWidgets
        , barPosition = Top
        , barPadding = 0
        , barHeight = 30
        , cssPath = cssFilePath
        }
      selectedConfig =
        fromMaybe baseConfig $ lookup hostName
          [ ( "uber-loaner"
            , baseConfig { endWidgets = shortLaptopEndWidgets }
            )
          , ( "imalison-home"
            , baseConfig { endWidgets = fullEndWidgets, barHeight = 42 }
            )
          , ( "ryzen-shine"
            , baseConfig { endWidgets = fullEndWidgets, barHeight = 42 }
            )
          , ( "ivanm-dfinity-razer"
            , baseConfig { endWidgets = shortLaptopEndWidgets, barHeight = 42 }
            )
          ]
      simpleTaffyConfig = selectedConfig
        { centerWidgets = []
        -- , endWidgets = []
        -- , startWidgets = []
        }
  startTaffybar $
    appendHook notifySystemD $
    appendHook (void $ getHost False) $
    withLogServer $
    withToggleServer $
    toTaffyConfig simpleTaffyConfig
