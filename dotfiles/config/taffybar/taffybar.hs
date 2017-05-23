import qualified Control.Concurrent.MVar as MV
import           Control.Exception.Base
import           Control.Monad
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.UI.Gtk.Layout.Table as T
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.Information.CPU
import           System.Information.Memory
import           System.Process
import           System.Taffybar
import           System.Taffybar.LayoutSwitcher
import           System.Taffybar.MPRIS2
import           System.Taffybar.NetMonitor
import           System.Taffybar.Pager
import           System.Taffybar.SimpleClock
import           System.Taffybar.Systray
import           System.Taffybar.TaffyPager
import           System.Taffybar.Widgets.PollingGraph
import           System.Taffybar.WindowSwitcher
import           System.Taffybar.WorkspaceHUD
import           Text.Printf
import           Text.Read hiding (get)
import           System.Taffybar.ToggleMonitor
import           XMonad.Core ( whenJust )


memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

underlineWidget cfg buildWidget name = do
  w <- buildWidget
  t <- T.tableNew 2 1 False
  u <- Gtk.eventBoxNew

  W.widgetSetSizeRequest u (-1) $ underlineHeight cfg

  T.tableAttach t w 0 1 0 1 [T.Expand] [T.Expand] 0 0
  T.tableAttach t u 0 1 1 2 [T.Fill] [T.Shrink] 0 0

  Gtk.widgetSetName u (printf "%s-underline" name :: String)

  Gtk.widgetShowAll t

  return $ Gtk.toWidget t

movableWidget builder =
  do
    -- Delay creation of the widget or else failure from trying to get screen
    widVar <- MV.newEmptyMVar
    let moveWidget = do
          isEmpty <- MV.isEmptyMVar widVar
          when isEmpty $
               do
                 putwid <- builder
                 MV.putMVar widVar putwid
          wid <- MV.readMVar widVar
          hbox <- Gtk.hBoxNew False 0
          parent <- Gtk.widgetGetParent wid
          if isJust parent
          then
            Gtk.widgetReparent wid hbox
          else
            Gtk.containerAdd hbox wid
          Gtk.widgetShowAll hbox
          return $ Gtk.toWidget hbox
    return moveWidget

getInterfaces = do
  (_, output, _) <- readCreateProcessWithExitCode (shell "list_interfaces.sh") ""
  return $ splitOn "\n" output

main = do
  monEither <-
    (try $ getEnv "TAFFYBAR_MONITOR") :: IO (Either SomeException String)
  interfaceNames <- getInterfaces
  homeDirectory <- getHomeDirectory
  let resourcesDirectory file =
        homeDirectory </> ".lib" </> "resources" </> file
      fallbackIcons _ klass
        | "URxvt" `isInfixOf` klass =
          IIFilePath $ resourcesDirectory "urxvt.png"
        | "Termite" `isInfixOf` klass =
          IIFilePath $ resourcesDirectory "urxvt.png"
        | "Kodi" `isInfixOf` klass = IIFilePath $ resourcesDirectory "kodi.png"
        | otherwise = IIColor (0xFF, 0xFF, 0, 0xFF)
      myGetIconInfo = windowTitleClassIconGetter False fallbackIcons
      (monFilter, monNumber) =
        case monEither of
          Left _ -> (allMonitors, 0)
          Right monString ->
            case readMaybe monString of
              Nothing -> (allMonitors, 0)
              Just num -> (useMonitorNumber, num)
      memCfg =
        defaultGraphConfig
        {graphDataColors = [(0.129, 0.588, 0.953, 1)], graphLabel = Just "mem"}
      cpuCfg =
        defaultGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
        , graphLabel = Just "cpu"
        }
      clock = textClockNew Nothing "%a %b %_d %r" 1
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = do
        theTray <- systrayNew
        cont <- Gtk.eventBoxNew
        Gtk.containerAdd cont theTray
        Gtk.widgetSetName cont "Taffytray"
        Gtk.widgetSetName theTray "Taffytray"
        Gtk.widgetShowAll cont
        return $ Gtk.toWidget cont
      hudConfig =
        defaultWorkspaceHUDConfig
        { underlineHeight = 3
        , underlinePadding = 5
        , minWSWidgetSize = Nothing
        , minIcons = 3
        , getIconInfo = myGetIconInfo
        , windowIconSize = 25
        , widgetGap = 0
        -- , widgetBuilder = buildBorderButtonController
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , updateIconsOnTitleChange = True
        , updateOnWMIconChange = True
        , debugMode = False
        , redrawIconsOnStateChange = True
        , innerPadding = 5
        , outerPadding = 5
        }
      netMonitor = netMonitorMultiNew 1.5 interfaceNames
      pagerConfig = defaultPagerConfig {useImages = True}
      pager = taffyPagerNew pagerConfig
      makeUnderline = underlineWidget hudConfig
  pgr <- pagerNew pagerConfig
  tray2 <- movableWidget tray

  let hud = buildWorkspaceHUD hudConfig pgr
      los = makeUnderline (layoutSwitcherNew pgr) "red"
      wnd = makeUnderline (windowSwitcherNew pgr) "teal"
      taffyConfig =
        defaultTaffybarConfig
        { startWidgets = [hud, los, wnd]
        , endWidgets =
            [ makeUnderline tray "yellow"
            , makeUnderline clock "teal"
            , makeUnderline mem "blue"
            , makeUnderline cpu "green"
            , makeUnderline netMonitor "yellow"
            , makeUnderline mpris "red"
            ]
        , monitorNumber = monNumber
        , barPosition = Top
        , barHeight = 50
        , widgetSpacing = 5
        }

  withToggleSupport taffyConfig

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
