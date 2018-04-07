{-# LANGUAGE PackageImports #-}
module Main where

import qualified        Control.Concurrent.MVar as MV
import                  Control.Exception.Base
import                  Control.Monad
import                  Control.Monad.Reader
import                  Data.GI.Base
import                  Data.GI.Base.ManagedPtr
import                  Data.List
import                  Data.List.Split
import qualified        Data.Map as M
import                  Data.Maybe
import                  Foreign.ForeignPtr
import                  Foreign.Ptr
import qualified        GI.Gtk as GI
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified "gtk3" Graphics.UI.Gtk.Abstract.Widget as W
import qualified "gtk3" Graphics.UI.Gtk.Layout.Table as T
import                  Graphics.UI.Gtk.Types
import                  StatusNotifier.Tray
import                  System.Directory
import                  System.Environment
import                  System.FilePath.Posix
import                  System.Glib.GObject
import                  System.Information.CPU
import                  System.Information.EWMHDesktopInfo
import                  System.Information.Memory
import                  System.Information.X11DesktopInfo
import                  System.Process
import                  System.Taffybar
import                  System.Taffybar.Battery
import                  System.Taffybar.IconImages
import                  System.Taffybar.LayoutSwitcher
import                  System.Taffybar.MPRIS2
import                  System.Taffybar.NetMonitor
import                  System.Taffybar.Pager
import                  System.Taffybar.SimpleClock
import                  System.Taffybar.Systray
import                  System.Taffybar.ToggleMonitor
import                  System.Taffybar.Widgets.PollingGraph
import                  System.Taffybar.WindowSwitcher
import                  System.Taffybar.WorkspaceHUD
import                  Text.Printf
import                  Text.Read hiding (lift)
import                  Unsafe.Coerce

data ConstantIconController = ConstantIconController { cicImage :: Gtk.Image }

instance WorkspaceWidgetController ConstantIconController where
  updateWidget cic _ = return cic
  getWidget = Gtk.toWidget . cicImage

instance WorkspaceWidgetController Gtk.Widget where
  updateWidget w _ = return w
  getWidget w = w

makeContents waction klass = do
  widget <- waction
  widgetSetClass widget "Contents"
  widgetSetClass widget klass
  b <- buildPadBox widget
  Gtk.widgetShowAll b
  return $ Gtk.toWidget b

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  }

memCfg =
  myGraphConfig
  {graphDataColors = [(0.129, 0.588, 0.953, 1)], graphLabel = Just "mem"}

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

getFullWorkspaceNames :: X11Property [(WorkspaceIdx, String)]
getFullWorkspaceNames = go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where go = zip [WSIdx i | i <- [0..]]

workspaceNamesLabelSetter workspace =
  fromMaybe "" . lookup (workspaceIdx workspace) <$>
            liftX11Def [] getFullWorkspaceNames

-- mem :: IO Gtk.Widget
-- mem = do
--   ebox <- Gtk.eventBoxNew
--   btn <- pollingGraphNew memCfg 1 $ memCallback $ Gtk.toWidget ebox
--   Gtk.containerAdd ebox btn
--   _ <- Gtk.on ebox Gtk.buttonPressEvent systemEvents
--   Gtk.widgetShowAll ebox
--   return $ Gtk.toWidget ebox

systemEvents :: Gtk.EventM Gtk.EButton Bool
systemEvents = return True

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

containerAddReturn c w =
  Gtk.containerAdd c w >> Gtk.widgetShowAll c >> (return $ Gtk.toWidget c)

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

myFormatEntry wsNames ((ws, wtitle, wclass), _) =
  printf "%s: %s - %s" wsName (head $ splitOn "\NUL" wclass) wtitle
  where
    wsName = M.findWithDefault ("WS#" ++ show wsN) ws wsNames
    WSIdx wsN = ws

getInterfaces = do
  (_, output, _) <- readCreateProcessWithExitCode (shell "list_interfaces.sh") ""
  return $ splitOn "\n" output

addClass klass action = do
  widget <- action
  widgetSetClass widget klass
  return widget

(buildWidgetCons, _) = mkWidget

buildSNITray = do
  GI.Widget trayGIWidgetMP <- buildTrayWithHost GI.OrientationHorizontal
  wrapNewGObject mkWidget (castPtr <$> disownManagedPtr trayGIWidgetMP)

main = do
  interfaceNames <- getInterfaces
  homeDirectory <- getHomeDirectory
  let resourcesDirectory = homeDirectory </> ".lib" </> "resources"
      inResourcesDirectory file = resourcesDirectory </> file
      highContrastDirectory =
        "/" </> "usr" </> "share" </> "icons" </> "HighContrast" </> "256x256"
      inHighContrastDirectory file = highContrastDirectory </> file
      getWorkspacePixBuf size Workspace {workspaceIdx = WSIdx wsId} =
        pixBufFromFile size . inHighContrastDirectory <$>
        case wsId + 1 of
          -- 1 -> Just $ "apps" </> "utilities-terminal.png"
          -- 2 -> Just $ "emblems" </> "emblem-documents.png"
          -- 3 -> Just $ "actions" </> "bookmark-add.png"
          -- 4 -> Just $ "devices" </> "video-display.png"
          _ -> Nothing
      buildConstantIconController :: ControllerConstructor
      buildConstantIconController ws = do
        cfg <- asks hudConfig
        lift $ do
          img <- Gtk.imageNew
          pb <- sequence $ getWorkspacePixBuf (windowIconSize cfg) ws
          setImage (windowIconSize cfg) img pb
          return $ WWC ConstantIconController {cicImage = img}
      makeIcon = return . IIFilePath . inResourcesDirectory
      myGetIconInfo w@WindowData {windowTitle = title, windowClass = klass}
        | "URxvt" `isInfixOf` klass = makeIcon "urxvt.png"
        | "Termite" `isInfixOf` klass = makeIcon "urxvt.png"
        | "Kodi" `isInfixOf` klass = makeIcon "kodi.png"
        | "@gmail.com" `isInfixOf` title &&
            "chrome" `isInfixOf` klass && "Gmail" `isInfixOf` title =
          makeIcon "gmail.png"
        | otherwise = do
          res <- defaultGetIconInfo w
          return $
            case res of
              IINone -> IIFilePath $ inResourcesDirectory "exe-icon.png"
              _ -> res
      cpuCfg =
        myGraphConfig
        { graphDataColors = [(0, 1, 0, 1), (1, 0, 1, 0.5)]
        , graphLabel = Just "cpu"
        }
      clock = textClockNew Nothing "%a %b %_d %r" 1
      mpris = mpris2New
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      myHUDConfig =
        defaultWorkspaceHUDConfig
        { underlineHeight = 3
        , underlinePadding = 2
        , minWSWidgetSize = Nothing
        , minIcons = 1
        , getIconInfo = myGetIconInfo
        , windowIconSize = 30
        , widgetGap = 0
        -- , widgetBuilder =
        --     buildButtonController $
        --     buildUnderlineController $
        --     buildContentsController
        --       [ buildConstantIconController
        --       , buildLabelController
        --       , buildIconController
        --       ]
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , updateOnWMIconChange = True
        , debugMode = False
        , labelSetter = workspaceNamesLabelSetter
        }
      netMonitor = netMonitorMultiNew 1.5 interfaceNames
      pagerConfig =
        defaultPagerConfig
        {useImages = True, windowSwitcherFormatter = myFormatEntry}
      -- makeUnderline = underlineWidget myHUDConfig
  pgr <- pagerNew pagerConfig
  -- tray2 <- movableWidget tray
  let hud = buildWorkspaceHUD myHUDConfig pgr
      los = layoutSwitcherNew pgr
      wnd = windowSwitcherNew pgr
      taffyConfig =
        defaultTaffybarConfig
        { startWidgets = [hud, los, addClass "WindowSwitcher" wnd]
        , endWidgets =
            [ batteryBarNew defaultBatteryConfig 1.0
            , makeContents clock "Cpu"
            -- , makeContents systrayNew "Cpu"
            , makeContents buildSNITray "Cpu"
            , makeContents cpu "Cpu"
            , makeContents mem "Cpu"
            , makeContents netMonitor "Cpu"
            , makeContents (join $ containerAddReturn <$> Gtk.eventBoxNew <*> mpris) "Cpu"
            ]
        , barPosition = Top
        , barPadding = 5
        , barHeight = (underlineHeight myHUDConfig + windowIconSize myHUDConfig + 15)
        , widgetSpacing = 0
        }
  withToggleSupport taffyConfig

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
