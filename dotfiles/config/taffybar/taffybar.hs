module Main where

import           Data.Char (toLower)

import           Data.List
import           Data.Maybe
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.UI.Gtk.Layout.Table as T
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Information.CPU
import           System.Information.Memory
import           System.Taffybar
import           System.Taffybar.IconImages
import           System.Taffybar.MPRIS2
import           System.Taffybar.Pager
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager
import           System.Taffybar.Widgets.PollingGraph
import           System.Taffybar.WorkspaceHUD
import           Text.Printf
import           Text.Read

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

underlineWidget buildWidget name = do
  w <- buildWidget
  t <- T.tableNew 2 1 False
  u <- Gtk.eventBoxNew

  W.widgetSetSizeRequest u (-1) $ 2

  T.tableAttach t w 0 1 0 1 [T.Expand] [T.Expand] 0 0
  T.tableAttach t u 0 1 1 2 [T.Fill] [T.Shrink] 0 0

  Gtk.widgetSetName u $ (printf "%s-underline" name :: String)

  Gtk.widgetShowAll t

  return $ Gtk.toWidget t

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
    , endWidgets = [underlineWidget tray "tray", underlineWidget clock "clock", mem, cpu, mpris]
    , monitorNumber = monitorNumber
    , monitorFilter = allMonitors
    , barPosition = Top
    , barHeight = 40
    }

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
