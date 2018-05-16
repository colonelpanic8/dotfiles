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
import                  System.Taffybar.Compat.GtkLibs
import                  System.Taffybar.DBus
import                  System.Taffybar.DBus.Toggle
import                  System.Taffybar.Hooks
import                  System.Taffybar.IconImages
import                  System.Taffybar.Information.CPU
import                  System.Taffybar.Information.EWMHDesktopInfo
import                  System.Taffybar.Information.Memory
import                  System.Taffybar.Information.X11DesktopInfo
import                  System.Taffybar.SimpleConfig
import                  System.Taffybar.Widget
import                  System.Taffybar.Widget.Generic.PollingGraph
import                  System.Taffybar.Widget.Generic.PollingLabel
import                  System.Taffybar.Widget.Util
import                  System.Taffybar.Widget.Workspaces
import                  Text.Printf
import                  Text.Read hiding (lift)
import                  Unsafe.Coerce

buildPadBoxNoShrink orig  = liftIO $ do
  widget <- buildPadBox orig
  widgetSetClass orig "Contents"
  -- toGIWidget widget >>= widgetPreventShrink
  return widget

setMinWidth width widget = liftIO $ do
  Gtk.widgetSetSizeRequest widget width (-1)
  return widget

makeContents waction klass = do
  widget <- waction
  liftIO $ do
    widgetSetClass widget "Contents"
    widgetSetClass widget klass
    b <- buildPadBox widget
    Gtk.widgetShowAll b
    return $ Gtk.toWidget b

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
  , graphBackgroundColor = (1.0, 1.0, 1.0, 0.0)
  }

netCfg =
  myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net"
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
  workspacesLogger <- getLogger "System.Taffybar.Widget.Workspaces"
  saveGlobalLogger $ setLevel WARNING workspacesLogger

github = do
  Right (token, _) <- passGet "github-token"
  githubNotificationsNew $ defaultGithubConfig $ Auth.OAuth $ BS.pack token

main = do
  interfaceNames <- getInterfaces
  homeDirectory <- getHomeDirectory
  -- logDebug
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
        , graphBackgroundColor = (1.0, 1.0, 1.0, 0.0)
        , graphLabel = Just "cpu"
        }
      clock = textClockNew Nothing "%a %b %_d %r" 1
      mpris = mpris2New
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      battery = batteryBarNewWithFormat defaultBatteryConfig
                "$percentage$% ($time$) - $status$" 1.0
      myWorkspacesConfig =
        defaultWorkspacesConfig
        { underlineHeight = 3
        , underlinePadding = 2
        , minWSWidgetSize = Nothing
        , minIcons = 1
        , getIconInfo = myGetIconInfo
        , windowIconSize = 25
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter = workspaceNamesLabelSetter
        }
      baseConfig = defaultSimpleTaffyConfig
        { startWidgets =
            [ workspaces
            , los >>= buildPadBox
            , wnd >>= buildPadBox
            ]
        , endWidgets = map (>>= buildPadBoxNoShrink)
          [ battery
          , clock >>= setMinWidth 200
          , sniTrayNew
          , github
          , cpu
          , mem
          , netMonitorGraphNew netCfg Nothing
          -- , networkMonitorNew defaultNetFormat Nothing >>= setMinWidth 200
          , fsMonitorNew 60 ["/dev/sdd2"]
          , mpris
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = (windowIconSize myWorkspacesConfig + 25)
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
  dyreTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
               toTaffyConfig simpleTaffyConfig

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
