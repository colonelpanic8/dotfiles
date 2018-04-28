{-# LANGUAGE PackageImports #-}
module Main where

import qualified        Control.Concurrent.MVar as MV
import                  Control.Exception.Base
import                  Control.Monad
import                  Control.Monad.Reader
import                  Control.Monad.Trans
import qualified        Data.ByteString.Char8 as BS
import                  Data.GI.Base
import                  Data.GI.Base.ManagedPtr
import                  Data.List
import                  Data.List.Split
import qualified        Data.Map as M
import                  Data.Maybe
import                  Debug.Trace
import                  Foreign.ForeignPtr
import                  Foreign.Ptr
import qualified        GI.Gtk as GI
import qualified        GitHub.Auth as Auth
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import qualified "gtk3" Graphics.UI.Gtk.Abstract.Widget as W
import qualified "gtk3" Graphics.UI.Gtk.Layout.Table as T
import                  Graphics.UI.Gtk.Types
import                  StatusNotifier.Tray
import                  System.Directory
import                  System.Environment
import                  System.FilePath.Posix
import                  System.Glib.GObject
import                  System.IO
import                  System.Log.Handler.Simple
import                  System.Log.Logger
import                  System.Process
import                  System.Taffybar
import                  System.Taffybar.Auth
import                  System.Taffybar.DBus.Toggle
import                  System.Taffybar.IconImages
import                  System.Taffybar.Information.CPU
import                  System.Taffybar.Information.EWMHDesktopInfo
import                  System.Taffybar.Information.Memory
import                  System.Taffybar.Information.X11DesktopInfo
import                  System.Taffybar.SimpleConfig
import                  System.Taffybar.Widget
import                  System.Taffybar.Widget.Generic.PollingGraph
import                  System.Taffybar.Widget.Workspaces
import                  Text.Printf
import                  Text.Read hiding (lift)
import                  Unsafe.Coerce

makeContents waction klass = do
  widget <- waction
  liftIO $ do
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
  { graphDataColors = [(0.129, 0.588, 0.953, 1)]
  , graphLabel = Just "mem"
  }

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

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

containerAddReturn c w =
  Gtk.containerAdd c w >> Gtk.widgetShowAll c >> return (Gtk.toWidget c)

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
  lift $ widgetSetClass widget klass
  return widget

(buildWidgetCons, _) = mkWidget

logDebug = do
  handler <- streamHandler stdout DEBUG
  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel DEBUG logger
  infoLogger <- getLogger "System.Information"
  saveGlobalLogger $ setLevel DEBUG infoLogger

github = do
  Right (token, _) <- passGet "github-token"
  githubNotificationsNew GitHubConfig { ghAuth = Auth.OAuth $ BS.pack token
                                      , ghIcon = undefined
                                      }

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
      -- buildConstantIconController :: ControllerConstructor
      -- buildConstantIconController ws = do
      --   cfg <- asks hudConfig
      --   lift $ do
      --     img <- Gtk.imageNew
      --     pb <- sequence $ getWorkspacePixBuf (windowIconSize cfg) ws
      --     setImage (windowIconSize cfg) img pb
      --     return $ WWC ConstantIconController {cicImage = img}
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
      myWorkspacesConfig =
        defaultWorkspacesConfig
        { underlineHeight = 3
        , underlinePadding = 2
        , minWSWidgetSize = Nothing
        , minIcons = 1
        , getIconInfo = myGetIconInfo
        , windowIconSize = 30
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter = workspaceNamesLabelSetter
        }
      netMonitor = netMonitorMultiNew 1.5 interfaceNames
      baseConfig = defaultSimpleTaffyConfig
        { startWidgets =
            [ workspaces
            , makeContents los "Layout"
            , makeContents wnd "Windows"
            ]
        , endWidgets =
          [ batteryBarNewWithFormat defaultBatteryConfig "$percentage$% ($time$) - $status$" 1.0
          , makeContents sniTrayNew "Cpu"
          , makeContents clock "Cpu"
          , makeContents cpu "Cpu"
          , makeContents mem "Cpu"
          , makeContents netMonitor "Cpu"
          , makeContents (fsMonitorNew 60 ["/dev/sdd2"]) "Cpu"
          , mpris >>= buildPadBox
          , github >>= buildPadBox
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = (underlineHeight myWorkspacesConfig + windowIconSize myWorkspacesConfig + 15)
        , widgetSpacing = 0
        }
      workspaces = workspacesNew myWorkspacesConfig
      los = layoutNew defaultLayoutConfig
      wnd = windowsNew defaultWindowsConfig
      simpleTaffyConfig =
        baseConfig
        -- { startWidgets = [workspaces]
        -- , centerWidgets = [makeContents (addClass "Window" wnd) "Cpu"]
        -- , endWidgets = [makeContents los "Cpu"]
        -- }
  dyreTaffybar $ handleDBusToggles $ toTaffyConfig simpleTaffyConfig

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
