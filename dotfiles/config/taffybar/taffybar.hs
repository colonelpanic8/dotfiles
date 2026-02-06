{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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
import qualified Data.Text
import           Data.Time
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Overlay as Gtk
import           Network.HostName
import           StatusNotifier.Tray
import           System.Directory
import           System.Environment
import           System.Environment.XDG.BaseDir
import           System.FilePath.Posix
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Process
import           System.Taffybar
import           System.Taffybar.Context (Backend(..), TaffyIO, appendHook, detectBackend)
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
import           System.Taffybar.Widget.Generic.Icon
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
import           System.Taffybar.Widget.Util
import qualified System.Taffybar.Widget.HyprlandWorkspaces as Hyprland
import qualified System.Taffybar.Widget.Workspaces as X11Workspaces
import           System.Taffybar.WindowIcon (pixBufFromColor)
import           Text.Printf
import           Text.Read hiding (lift)
import           Data.Int (Int32)

setClassAndBoundingBoxes :: MonadIO m => Data.Text.Text -> Gtk.Widget -> m Gtk.Widget
setClassAndBoundingBoxes klass = buildContentsBox >=> flip widgetSetClassGI klass

deocrateWithSetClassAndBoxes :: MonadIO m => Data.Text.Text -> m Gtk.Widget -> m Gtk.Widget
deocrateWithSetClassAndBoxes klass builder = builder >>= setClassAndBoundingBoxes klass

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
  { graphDataColors = [red, (1, 0, 1, 0.5)]
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
  remapNSP . fromMaybe "" . lookup (X11Workspaces.workspaceIdx workspace) <$>
            liftX11Def [] getFullWorkspaceNames
  where remapNSP "NSP" = "S"
        remapNSP n = n

enableLogger logger level = do
  logger <- getLogger logger
  saveGlobalLogger $ setLevel level logger

-- Systemd --user's manager environment can be stale across logins (e.g. still
-- containing WAYLAND_DISPLAY from an older session). `detectBackend` will pick
-- Wayland if WAYLAND_DISPLAY is set, even when no compositor is running.
--
-- Prefer a backend that's actually usable, and sanitize the process environment
-- so taffybar's internal context backend detection makes the same decision.
detectBackendRobust :: IO Backend
detectBackendRobust = do
  mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  mWaylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
  mDisplay <- lookupEnv "DISPLAY"
  mSessionType <- lookupEnv "XDG_SESSION_TYPE"
  mHyprSig <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"

  let mWaylandPath = do
        runtime <- mRuntime
        wl <- mWaylandDisplay
        guard (not (null runtime) && not (null wl))
        pure (runtime </> wl)

  waylandOk <- case mWaylandPath of
    Nothing -> pure False
    Just wlPath -> do
      ok <- doesPathExist wlPath
      when (not ok) $
        logM "Main" DEBUG $
          "WAYLAND_DISPLAY is set but no socket at " ++ wlPath ++ "; preferring X11 when available"
      pure ok

  when (not waylandOk && maybe False (not . null) mDisplay) $ do
    when (isJust mWaylandDisplay) $ unsetEnv "WAYLAND_DISPLAY"
    when (isJust mHyprSig) $ unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
    when (mSessionType == Just "wayland") $ setEnv "XDG_SESSION_TYPE" "x11"

  case () of
    _ | waylandOk -> pure BackendWayland
      | maybe False (not . null) mDisplay -> pure BackendX11
      | otherwise -> detectBackend

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

iconRemap :: [(Data.Text.Text, [Data.Text.Text])]
iconRemap =
  [ ("spotify", ["spotify-client", "spotify"])
  ]

iconRemapMap :: M.Map Data.Text.Text [Data.Text.Text]
iconRemapMap =
  M.fromList [ (Data.Text.toLower k, v) | (k, v) <- iconRemap ]

lookupIconRemap :: Data.Text.Text -> [Data.Text.Text]
lookupIconRemap name = fromMaybe [] $ M.lookup (Data.Text.toLower name) iconRemapMap

iconNameVariants :: Data.Text.Text -> [Data.Text.Text]
iconNameVariants raw =
  let lower = Data.Text.toLower raw
      stripped = fromMaybe lower (Data.Text.stripSuffix ".desktop" lower)
      suffixes = ["-gtk", "-client", "-desktop"]
      stripSuffixes name =
        let variants = mapMaybe (`Data.Text.stripSuffix` name) suffixes
        in nub $ variants ++ [name]
      baseNames = stripSuffixes stripped ++ [raw]
      toDash c
        | c == ' ' || c == '_' || c == '.' || c == '/' = '-'
        | otherwise = c
      toUnderscore c
        | c == ' ' || c == '-' || c == '.' || c == '/' = '_'
        | otherwise = c
      variantsFor name =
        let dotted =
              case Data.Text.splitOn "." name of
                [] -> name
                xs -> last xs
            dashed = Data.Text.map toDash name
            dashedDotted = Data.Text.map toDash dotted
            underscored = Data.Text.map toUnderscore name
            underscoredDotted = Data.Text.map toUnderscore dotted
        in [dotted, dashed, dashedDotted, underscored, underscoredDotted, name]
  in nub $ concatMap variantsFor baseNames

-- Hyprland "special" workspaces (e.g. "special:slack") are scratchpad-like and
-- usually not something we want visible in the workspace widget.
isSpecialHyprWorkspace :: Hyprland.HyprlandWorkspace -> Bool
isSpecialHyprWorkspace ws =
  let name = Data.Text.toLower $ Data.Text.pack $ Hyprland.workspaceName ws
  in Data.Text.isPrefixOf "special" name || Hyprland.workspaceIdx ws < 0

hyprlandIconCandidates :: Hyprland.HyprlandWindow -> [Data.Text.Text]
hyprlandIconCandidates windowData =
  let baseNames = map Data.Text.pack $ catMaybes
        [ Hyprland.windowClass windowData
        , Hyprland.windowInitialClass windowData
        ]
      remapped = concatMap lookupIconRemap baseNames
      remappedExpanded = concatMap iconNameVariants remapped
      baseExpanded = concatMap iconNameVariants baseNames
  in nub (remappedExpanded ++ baseExpanded)

isPathCandidate :: Data.Text.Text -> Bool
isPathCandidate name =
  Data.Text.isInfixOf "/" name ||
  any (`Data.Text.isSuffixOf` name) [".png", ".svg", ".xpm"]

hyprlandIconFromCandidate size name
  | isPathCandidate name =
      liftIO $ getPixbufFromFilePath (Data.Text.unpack name)
  | otherwise =
      maybeTCombine
        (Hyprland.getWindowIconFromDesktopEntryByAppId size (Data.Text.unpack name))
        (liftIO $ loadPixbufByName size name)

hyprlandManualIconGetter :: Hyprland.HyprlandWindowIconPixbufGetter
hyprlandManualIconGetter =
  Hyprland.handleIconGetterException $ \size windowData ->
    foldl maybeTCombine (return Nothing) $
      map (hyprlandIconFromCandidate size) (hyprlandIconCandidates windowData)

fallbackIconPixbuf :: Int32 -> TaffyIO (Maybe Gdk.Pixbuf)
fallbackIconPixbuf size = do
  let fallbackNames =
        [ "application-x-executable"
        , "application"
        , "image-missing"
        , "gtk-missing-image"
        , "dialog-question"
        , "utilities-terminal"
        , "system-run"
        , "window"
        ]
      tryNames =
        foldl
          maybeTCombine
          (return Nothing)
          (map (liftIO . loadPixbufByName size) fallbackNames)
  result <- tryNames
  case result of
    Just _ -> return result
    Nothing -> Just <$> pixBufFromColor size 0x5f5f5fff

hyprlandFallbackIcon :: Hyprland.HyprlandWindowIconPixbufGetter
hyprlandFallbackIcon size _ =
  fallbackIconPixbuf size

cssFilesByHostname =
  [ ("uber-loaner", ["palette.css", "uber-loaner.css"])
  , ("imalison-home", ["palette.css", "taffybar.css"])
  , ("ivanm-dfinity-razer", ["palette.css", "taffybar.css"])
  , ("ryzen-shine", ["palette.css", "taffybar.css"])
  , ("stevie-nixos", ["palette.css", "taffybar.css"])
  ]

main = do
  enableLogger "Graphics.UI.GIGtkStrut" DEBUG

  hostName <- getHostName
  backend <- detectBackendRobust
  let relativeFiles = fromMaybe ["palette.css", "taffybar.css"] $ lookup hostName cssFilesByHostname
  cssFiles <- mapM (getUserConfigFile "taffybar") relativeFiles

  let myCPU =
        ( deocrateWithSetClassAndBoxes "cpu" $
            pollingGraphNew cpuCfg 5 cpuCallback
        ) :: TaffyIO Gtk.Widget
      myMem =
        ( deocrateWithSetClassAndBoxes "mem" $
            pollingGraphNew memCfg 5 memCallback
        ) :: TaffyIO Gtk.Widget
      myNet =
        ( deocrateWithSetClassAndBoxes "net" $
            networkGraphNew netCfg Nothing
        ) :: TaffyIO Gtk.Widget
      myAudio = deocrateWithSetClassAndBoxes "audio" $
        PulseAudio.pulseAudioLabelNew
      myNetwork = deocrateWithSetClassAndBoxes "network" $
        NetworkManager.networkManagerWifiLabelNew
      myLayout = deocrateWithSetClassAndBoxes "layout" $
        layoutNew defaultLayoutConfig
      myWindows = deocrateWithSetClassAndBoxes "windows" $
        windowsNew defaultWindowsConfig
      myWorkspaces =
        flip widgetSetClassGI "workspaces" =<<
          X11Workspaces.workspacesNew X11Workspaces.defaultWorkspacesConfig
            { X11Workspaces.minIcons = 1
            , X11Workspaces.getWindowIconPixbuf =
              X11Workspaces.scaledWindowIconPixbufGetter $
                X11Workspaces.getWindowIconPixbufFromChrome <|||>
                X11Workspaces.unscaledDefaultGetWindowIconPixbuf <|||>
                (\size _ -> fallbackIconPixbuf size)
            , X11Workspaces.widgetGap = 0
            , X11Workspaces.showWorkspaceFn = X11Workspaces.hideEmpty
            , X11Workspaces.updateRateLimitMicroseconds = 100000
            , X11Workspaces.labelSetter = workspaceNamesLabelSetter
            , X11Workspaces.widgetBuilder = X11Workspaces.buildLabelOverlayController
            }
      myHyprWorkspaces =
        flip widgetSetClassGI "workspaces" =<<
          Hyprland.hyprlandWorkspacesNew Hyprland.defaultHyprlandWorkspacesConfig
            { Hyprland.widgetGap = 0
            , Hyprland.minIcons = 1
            -- Don't show Hyprland "special:*" workspaces.
            , Hyprland.showWorkspaceFn =
              (\ws -> Hyprland.workspaceState ws /= X11Workspaces.Empty &&
                      not (isSpecialHyprWorkspace ws))
            , Hyprland.getWindowIconPixbuf =
              hyprlandManualIconGetter <|||>
              Hyprland.defaultHyprlandGetWindowIconPixbuf <|||>
              hyprlandFallbackIcon
            }
      myClock = deocrateWithSetClassAndBoxes "clock" $
        textClockNewWith
          defaultClockConfig
          { clockUpdateStrategy = RoundedTargetInterval 60 0.0
          , clockFormatString = "%a %b %_d, 🕑%I:%M %p"
          }
      myMpris =
        mpris2NewWithConfig
          MPRIS2Config
            { mprisWidgetWrapper = deocrateWithSetClassAndBoxes "mpris" . return
            , updatePlayerWidget =
              simplePlayerWidget
                defaultPlayerConfig
                  { setNowPlayingLabel = playingText 20 20
                  }
            }
      myBatteryIcon = deocrateWithSetClassAndBoxes "battery-icon" batteryIconNew
      myBatteryText =
        deocrateWithSetClassAndBoxes "battery-text" $ textBatteryNew "$percentage$%"
      batteryWidgets = [ myBatteryIcon, myBatteryText ]
      baseEndWidgets = [ myAudio, myNetwork, myMpris ]
      laptopEndWidgets = batteryWidgets ++ baseEndWidgets
      startWidgetsForBackend =
        case backend of
          BackendX11 -> [ myWorkspaces, myLayout, myWindows ]
          BackendWayland -> [ myHyprWorkspaces ]
      baseConfig =
        defaultSimpleTaffyConfig
          { startWidgets = startWidgetsForBackend
          , endWidgets = baseEndWidgets
          , barPosition = Top
          , widgetSpacing = 0
          , barPadding = 4
          , barHeight = ScreenRatio $ 1 / 36
          , cssPaths = cssFiles
          , centerWidgets = [ myClock ]
          }
      hostOverrides =
        [ ("uber-loaner", \cfg -> cfg { endWidgets = laptopEndWidgets })
        , ("adell", \cfg -> cfg { endWidgets = laptopEndWidgets })
        , ("stevie-nixos", \cfg -> cfg { endWidgets = laptopEndWidgets })
        , ("strixi-minaj", \cfg -> cfg { endWidgets = laptopEndWidgets })
        , ("jay-lenovo", \cfg -> cfg { endWidgets = laptopEndWidgets })
        ]
      simpleTaffyConfig =
        fromMaybe baseConfig $ ($ baseConfig) <$> lookup hostName hostOverrides
  startTaffybar $
    withLogServer $
    withToggleServer $
    toTaffybarConfig simpleTaffyConfig
