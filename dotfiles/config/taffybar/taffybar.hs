{-# LANGUAGE PackageImports #-}
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
import qualified GitHub.Auth as Auth
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

getFullWorkspaceNames :: X11Property [(WorkspaceIdx, String)]
getFullWorkspaceNames = go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where go = zip [WSIdx i | i <- [0..]]

workspaceNamesLabelSetter workspace =
  fromMaybe "" . lookup (workspaceIdx workspace) <$>
            liftX11Def [] getFullWorkspaceNames

enableLogger logger level = do
  logger <- getLogger logger
  saveGlobalLogger $ setLevel level logger

logDebug = do
  logger <- getLogger "System.Taffybar.Widget.Generic.AutoSizeImage"
  saveGlobalLogger $ setLevel DEBUG logger
  logger2 <- getLogger "StatusNotifier.Tray"
  saveGlobalLogger $ setLevel DEBUG logger2
  workspacesLogger <- getLogger "System.Taffybar.Widget.Workspaces"
  saveGlobalLogger $ setLevel WARNING workspacesLogger

-- github = do
--   Right (token, _) <- passGet "github-token"
--   githubNotificationsNew $ defaultGithubConfig $ Auth.OAuth $ BS.pack token

main = do
  homeDirectory <- getHomeDirectory
  -- logDebug
  -- logM "What" WARNING "Why"
  -- enableLogger "System.Taffybar.Widget.Util" DEBUG
  -- enableLogger "System.Taffybar.Information.XDG.DesktopEntry" DEBUG
  -- enableLogger "System.Taffybar.WindowIcon" DEBUG
  let resourcesDirectory = homeDirectory </> ".lib" </> "resources"
      inResourcesDirectory file = resourcesDirectory </> file
      highContrastDirectory =
        "/" </> "usr" </> "share" </> "icons" </> "HighContrast" </> "256x256"
      inHighContrastDirectory file = highContrastDirectory </> file
      getIconFileName w@WindowData {windowTitle = title, windowClass = klass}
        -- | "URxvt" `isInfixOf` klass = Just "urxvt.png"
        -- | "Termite" `isInfixOf` klass = Just "urxvt.png"
        -- | "Kodi" `isInfixOf` klass = Just "kodi.png"
        | "@gmail.com" `isInfixOf` title &&
            "chrome" `isInfixOf` klass && "Gmail" `isInfixOf` title =
          Just "gmail.png"
        | otherwise = Nothing
      myIcons = scaledWindowIconPixbufGetter $
                unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> lift $ loadPixbufByName size "application-default-icon")
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]
      myWorkspacesConfig =
        defaultWorkspacesConfig
        { underlineHeight = 3
        , underlinePadding = 2
        , minIcons = 1
        , getWindowIconPixbuf = myIcons
        -- , windowIconSize = 31
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter = workspaceNamesLabelSetter
        }
      workspaces = workspacesNew myWorkspacesConfig
      baseConfig =
        defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [layout, windows]
        , endWidgets =
            map
              (>>= buildContentsBox)
              [ textBatteryNew "$percentage$%"
              , batteryIconNew
              , textClockNew Nothing "%a %b %_d %r" 1
              , sniTrayNew
          -- , github
              , cpu
              , mem
              , networkGraphNew netCfg Nothing
          -- , networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 200
          -- , fsMonitorNew 60 ["/dev/sdd2"]
              , mpris2New
              ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 45
        }
      simpleTaffyConfig = baseConfig
        -- { endWidgets = []
        -- , startWidgets = [flip widgetSetClass "Workspaces" =<< workspaces]
        -- }
  startTaffybar $
    appendHook notifySystemD $
    appendHook (void $ getHost False) $
    withBatteryRefresh $
    withLogServer $
    withToggleServer $
    toTaffyConfig simpleTaffyConfig

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
