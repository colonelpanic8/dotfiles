module Main where

import Data.Char (toLower)

import Text.Read
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import System.Information.CPU
import System.Information.Memory
import System.Taffybar
import System.Taffybar.IconImages
import System.Taffybar.MPRIS2
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.WorkspaceHUD
import System.Environment

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

resourcesDirectory file =  ("/home" </> "imalison" </> ".lib" </> "resources" </> file)

fallbackIcons _ klass
  | isInfixOf "URxvt" klass = IIFilePath $ resourcesDirectory "urxvt.png"
  | isInfixOf "Kodi" klass = IIFilePath $ resourcesDirectory "kodi.png"
  | otherwise = IIColor $ (0xFF, 0xFF, 0, 0xFF)

myGetIconInfo =
  windowTitleClassIconGetter False fallbackIcons

main = do
  monString <- getEnv "TAFFYBAR_MONITOR"
  let monitorNumber = fromMaybe 1 $ readMaybe monString
  let memCfg =
        defaultGraphConfig
        {graphDataColors = [(1, 0, 0, 1)], graphLabel = Just "mem"}
      cpuCfg =
        defaultGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
        , graphLabel = Just "cpu"
        }
  let clock = textClockNew Nothing "%a %b %_d %r" 1
      pagerConfig = defaultPagerConfig {useImages = True}
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      hudConfig =
        defaultWorkspaceHUDConfig
        { underlineHeight = 3
        , underlinePadding = 0
        , minWSWidgetSize = Nothing
        , minIcons = 3
        , getIconInfo = myGetIconInfo
        , windowIconSize = 32
        , widgetGap = 0
        -- , widgetBuilder = buildBorderButtonController
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , updateIconsOnTitleChange = True
        , updateOnWMIconChange = True
        , debugMode = True
        , redrawIconsOnStateChange = True
        }
      hudPagerConfig = hudFromPagerConfig pagerConfig
      hud = taffyPagerHUDNew pagerConfig hudConfig
      pager = taffyPagerNew pagerConfig
  defaultTaffybar
    defaultTaffybarConfig
    { startWidgets = [hud]
    , endWidgets = [tray, clock, mem, cpu, mpris]
    , monitorNumber = monitorNumber
    , monitorFilter = allMonitors
    , barPosition = Top
    , barHeight = 40
    }

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
