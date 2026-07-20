{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.Widgets
  ( clockWidget,
    endWidgetsForHost,
    startWidgetsForBackend,
    startWidgetsForHostAndBackend,
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified StatusNotifier.Tray as SNITray
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Process (callProcess)
import System.Taffybar.Context
  ( Backend (BackendWayland, BackendX11),
    TaffyIO,
  )
import System.Taffybar.Information.Memory
  ( MemoryInfo (..),
    getMemoryInfoChan,
    getMemoryInfoState,
  )
import qualified System.Taffybar.Information.Workspaces.Hyprland as HyprlandWorkspaces
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget
import qualified System.Taffybar.Widget.Audio as Audio
import qualified System.Taffybar.Widget.CPUFrequency as CPUFrequency
import System.Taffybar.Widget.CPUMonitor (cpuMonitorNewWithHover, cpuMonitorNewWithHoverAndPower)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.Graph (GraphConfig (..), GraphDirection (..), GraphStyle (..), defaultGraphConfig)
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import System.Taffybar.Widget.SNIMenu (withNmAppletMenu)
import System.Taffybar.Widget.SNITray
  ( CollapsibleSNITrayParams (..),
    SNITrayConfig (..),
    defaultCollapsibleSNITrayParams,
    defaultSNITrayConfig,
  )
import System.Taffybar.Widget.SNITray.PrioritizedCollapsible
  ( PrioritizedCollapsibleSNITrayParams (..),
    defaultPrioritizedCollapsibleSNITrayParams,
    sniTrayPrioritizedCollapsibleNewFromParams,
  )
import qualified System.Taffybar.Widget.ScreenLock as ScreenLock
import System.Taffybar.Widget.Util
  ( buildIconLabelBox,
    pixbufNewFromFileAtScaleByHeight,
    widgetSetClassGI,
  )
import qualified System.Taffybar.Widget.Wlsunset as Wlsunset
import qualified System.Taffybar.Widget.Workspaces as Workspaces
import TaffybarConfig.AIUsage (aiUsageWidget)
import TaffybarConfig.Host (laptopHosts)
import TaffybarConfig.Temperature (cpuGpuTemperatureWidget)
import TaffybarConfig.WidgetUtil
  ( decorateWithClassAndBox,
    decorateWithClassAndBoxM,
    setFixedLabelWidth,
    setLabelAlignmentRecursively,
    stackInPill,
    usageLogoWidget,
  )
import TaffybarConfig.Workspaces (workspaceLabelSetter, workspaceShowPredicate, workspaceWindowIconGetter)
import Text.Printf (printf)
import Text.Read (readMaybe)

systemTelemetryPollInterval :: Double
systemTelemetryPollInterval = 10

cpuGraphPollInterval :: Double
cpuGraphPollInterval = 5

cpuGraphHoverPollInterval :: Double
cpuGraphHoverPollInterval = 0.5

cpuPowerPollInterval :: Double
cpuPowerPollInterval = 1

audioWidget :: TaffyIO Gtk.Widget
audioWidget =
  decorateWithClassAndBoxM "audio" Audio.audioNew

networkInnerWidget :: TaffyIO Gtk.Widget
networkInnerWidget =
  withNmAppletMenu NetworkManager.networkManagerWifiIconLabelNew
    >>= flip widgetSetClassGI "network"

networkWidget :: TaffyIO Gtk.Widget
networkWidget =
  decorateWithClassAndBoxM "network" networkInnerWidget

layoutWidget :: TaffyIO Gtk.Widget
layoutWidget =
  decorateWithClassAndBoxM "layout" (layoutNew defaultLayoutConfig)

windowsWidget :: TaffyIO Gtk.Widget
windowsWidget =
  decorateWithClassAndBoxM
    "windows"
    ( windowsNew
        defaultWindowsConfig
          { getActiveLabel = truncatedGetActiveLabel 28,
            configureActiveLabel = liftIO . setFixedLabelWidth 28
          }
    )

workspacesWidget :: TaffyIO Gtk.Widget
workspacesWidget =
  Workspaces.workspacesNew cfg
  where
    cfg =
      Workspaces.defaultWorkspacesConfig
        { Workspaces.widgetGap = 0,
          Workspaces.minIcons = 1,
          Workspaces.getWindowIconPixbuf = workspaceWindowIconGetter,
          Workspaces.hyprlandWorkspaceProviderConfig =
            HyprlandWorkspaces.defaultHyprlandWorkspaceProviderConfig
              { HyprlandWorkspaces.specialWorkspaceWindowTarget =
                  HyprlandWorkspaces.specialWorkspaceWindowsToMinimized
              },
          Workspaces.labelSetter = workspaceLabelSetter,
          Workspaces.showWorkspaceFn = workspaceShowPredicate
        }

clockWidget :: TaffyIO Gtk.Widget
clockWidget = do
  clock <-
    textClockNewWith
      defaultClockConfig
        { clockUpdateStrategy = RoundedTargetInterval 60 0.0,
          clockFormatString = "%a %b %_d\n%I:%M %p"
        }
  liftIO $ setLabelAlignmentRecursively 0.5 Gtk.JustificationCenter clock
  decorateWithClassAndBox "clock" clock

singleLineMprisLabel :: Text -> Text
singleLineMprisLabel =
  T.replace "\n" " " . T.replace "\r" " "

stackedMprisLabel :: Text -> Text
stackedMprisLabel raw =
  let normalized = singleLineMprisLabel raw
      (top, rest) = T.breakOn " - " normalized
   in if T.null rest
        then normalized
        else top <> "\n" <> T.drop 3 rest

mprisWidget :: TaffyIO Gtk.Widget
mprisWidget =
  mpris2NewWithConfig
    MPRIS2Config
      { mprisWidgetWrapper = decorateWithClassAndBox "mpris",
        updatePlayerWidget =
          simplePlayerWidget
            defaultPlayerConfig
              { setNowPlayingLabel =
                  fmap stackedMprisLabel . playingText 20 20,
                setupPlayerLabel = setFixedLabelWidth 20
              }
      }

batteryInnerWidget :: TaffyIO Gtk.Widget
batteryInnerWidget = do
  iconWidget <- batteryTextIconNew
  labelWidget <- textBatteryNew "$percentage$% $signedWatts$W"
  liftIO (buildIconLabelBox iconWidget labelWidget) >>= flip widgetSetClassGI "battery"

batteryWidget :: TaffyIO Gtk.Widget
batteryWidget =
  decorateWithClassAndBoxM "battery" batteryInnerWidget

backlightWidget :: TaffyIO Gtk.Widget
backlightWidget =
  decorateWithClassAndBoxM
    "backlight"
    ( backlightLabelNewChanWith
        defaultBacklightWidgetConfig
          { backlightFormat = "☀ $percent$%",
            backlightUnknownFormat = "☀ n/a",
            backlightTooltipFormat =
              Just "Device: $device$\nBrightness: $brightness$/$max$ ($percent$%)"
          }
    )

diskUsageInnerWidget :: TaffyIO Gtk.Widget
diskUsageInnerWidget =
  diskUsageNew >>= flip widgetSetClassGI "disk-usage"

diskUsageWidget :: TaffyIO Gtk.Widget
diskUsageWidget =
  decorateWithClassAndBoxM "disk-usage" diskUsageInnerWidget

meminfoPercentRowWidget ::
  Text ->
  Text ->
  (MemoryInfo -> Maybe Double) ->
  (MemoryInfo -> T.Text) ->
  TaffyIO Gtk.Widget
meminfoPercentRowWidget rowClass iconText getRatio tooltipText = do
  chan <- getMemoryInfoChan systemTelemetryPollInterval
  initialInfo <- getMemoryInfoState systemTelemetryPollInterval
  liftIO $ do
    iconW <- Gtk.toWidget =<< Gtk.labelNew (Just iconText)
    valueLabel <- Gtk.labelNew (Just "")
    valueW <- Gtk.toWidget valueLabel
    row <- buildIconLabelBox iconW valueW
    _ <- widgetSetClassGI row rowClass
    renderedRef <- newIORef Nothing

    let fmtPercent :: Double -> T.Text
        fmtPercent r = T.pack (printf "%.0f%%" (max 0 r * 100))
        updateWidget :: MemoryInfo -> IO ()
        updateWidget info = do
          let valueText = maybe "n/a" fmtPercent (getRatio info)
              rendered = (valueText, tooltipText info)
          previous <- readIORef renderedRef
          when (previous /= Just rendered) $ do
            writeIORef renderedRef (Just rendered)
            postGUIASync $ do
              Gtk.labelSetText valueLabel valueText
              Gtk.widgetSetTooltipText row (Just (snd rendered))

    _ <- Gtk.onWidgetRealize row $ updateWidget initialInfo
    Gtk.toWidget =<< channelWidgetNew row chan updateWidget

ramRowWidget :: TaffyIO Gtk.Widget
ramRowWidget =
  meminfoPercentRowWidget
    "ram-row"
    "\xF538" -- Font Awesome: memory
    (Just . memoryUsedRatio)
    (\info -> "RAM  " <> showMemoryInfo "$used$/$total$" 2 info)

swapRowWidget :: TaffyIO Gtk.Widget
swapRowWidget =
  meminfoPercentRowWidget
    "swap-row"
    "\xF0EC" -- Font Awesome: exchange (swap-ish)
    (\info -> if memorySwapTotal info <= 0 then Nothing else Just (memorySwapUsedRatio info))
    (\info -> "SWAP " <> showMemoryInfo "$swapUsed$/$swapTotal$" 2 info)

ramSwapWidget :: TaffyIO Gtk.Widget
ramSwapWidget =
  stackInPill "ram-swap" [ramRowWidget, swapRowWidget]

audioBacklightWidget :: TaffyIO Gtk.Widget
audioBacklightWidget =
  stackInPill
    "audio-backlight"
    [ Audio.audioNew,
      backlightNewChanWith
        defaultBacklightWidgetConfig
          { backlightFormat = "$percent$%",
            backlightUnknownFormat = "n/a",
            backlightTooltipFormat =
              Just "Device: $device$\nBrightness: $brightness$/$max$ ($percent$%)"
          }
    ]

cpuFrequencyInnerWidget :: TaffyIO Gtk.Widget
cpuFrequencyInnerWidget =
  CPUFrequency.cpuFrequencyNewWithConfig
    CPUFrequency.defaultCPUFrequencyWidgetConfig
      { CPUFrequency.cpuFrequencyPollInterval = systemTelemetryPollInterval
      }

batteryNetworkWidget :: TaffyIO Gtk.Widget
batteryNetworkWidget =
  stackInPill "battery-network" [batteryInnerWidget, networkInnerWidget]

diskCPUFrequencyWidget :: TaffyIO Gtk.Widget
diskCPUFrequencyWidget =
  stackInPill "disk-cpu-frequency" [diskUsageInnerWidget, cpuFrequencyInnerWidget]

screenLockWidget :: TaffyIO Gtk.Widget
screenLockWidget =
  decorateWithClassAndBoxM "screen-lock" $
    ScreenLock.screenLockNewWithConfig
      ScreenLock.defaultScreenLockConfig
        { ScreenLock.screenLockIcon = T.pack "\xF023" <> " Lock"
        }

wlsunsetWidget :: TaffyIO Gtk.Widget
wlsunsetWidget =
  decorateWithClassAndBoxM "wlsunset" $
    Wlsunset.wlsunsetNewWithConfig
      Wlsunset.defaultWlsunsetWidgetConfig
        { Wlsunset.wlsunsetWidgetIcon = T.pack "\xF0599" <> " Sun"
        }

simplifiedScreenLockWidget :: TaffyIO Gtk.Widget
simplifiedScreenLockWidget =
  -- Inner widget: no extra pill wrapping (the combiner provides that).
  ScreenLock.screenLockNewWithConfig
    ScreenLock.defaultScreenLockConfig
      { ScreenLock.screenLockIcon = T.pack "\xF023" <> " Lock"
      }

simplifiedWlsunsetWidget :: TaffyIO Gtk.Widget
simplifiedWlsunsetWidget =
  -- Inner widget: no extra pill wrapping (the combiner provides that).
  Wlsunset.wlsunsetNewWithConfig
    Wlsunset.defaultWlsunsetWidgetConfig
      { Wlsunset.wlsunsetWidgetIcon = T.pack "\xF0599" <> " Sun"
      }

sunLockWidget :: TaffyIO Gtk.Widget
sunLockWidget =
  stackInPill "sun-lock" [simplifiedWlsunsetWidget, simplifiedScreenLockWidget]

cpuPowerOverlayHosts :: [String]
cpuPowerOverlayHosts = ["strixi-minaj"]

cpuWidget :: Bool -> TaffyIO Gtk.Widget
cpuWidget showPackagePower =
  decorateWithClassAndBoxM "cpu" $
    if showPackagePower
      then
        cpuMonitorNewWithHoverAndPower
          graphConfig
          cpuGraphPollInterval
          cpuGraphHoverPollInterval
          cpuPowerPollInterval
          "cpu"
      else
        cpuMonitorNewWithHover
          graphConfig
          cpuGraphPollInterval
          cpuGraphHoverPollInterval
          "cpu"
  where
    graphConfig =
      defaultGraphConfig
        { graphDataColors = [(0, 1, 0.5, 0.8), (1, 0, 0, 0.5)],
          graphBackgroundColor = (0, 0, 0, 0),
          graphBorderWidth = 0,
          graphLabel = Just "CPU",
          graphWidth = 50,
          graphDirection = LEFT_TO_RIGHT
        }

wakeupDebugWidget :: TaffyIO Gtk.Widget
wakeupDebugWidget =
  decorateWithClassAndBoxM "wakeup-debug" wakeupDebugWidgetNew

omniMenuItem :: Text -> Text -> Text -> OmniMenuItem
omniMenuItem label iconName command =
  OmniMenuItem
    { omniMenuItemLabel = label,
      omniMenuItemCommand = command,
      omniMenuItemIcon = Just iconName,
      omniMenuItemTooltip = Just command
    }

omniMenuWidget :: TaffyIO Gtk.Widget
omniMenuWidget =
  decorateWithClassAndBoxM "omni-menu" $ do
    icon <-
      liftIO $ do
        iconPath <- getUserConfigFile "taffybar" "icons/nix-snowflake.svg"
        pixbufNewFromFileAtScaleByHeight 18 iconPath >>= \case
          Right pixbuf -> Gtk.toWidget =<< Gtk.imageNewFromPixbuf (Just pixbuf)
          Left _ ->
            Gtk.imageNewFromIconName
              (Just "system-run")
              (fromIntegral $ fromEnum Gtk.IconSizeMenu)
              >>= Gtk.toWidget
    omniMenuNewWithConfig
      (defaultOmniMenuConfig icon)
        { omniMenuIncludeApplications = True,
          omniMenuSections =
            [ OmniMenuSection
                "Launch"
                [ omniMenuItem "App launcher" "view-app-grid-symbolic" "hypr_shell_ui launcher",
                  omniMenuItem "Run command" "system-run" "hypr_shell_ui run",
                  omniMenuItem "Terminal" "utilities-terminal" "ghostty --gtk-single-instance=false",
                  omniMenuItem "Window picker" "preferences-system-windows" "hypr_shell_ui window go"
                ],
              OmniMenuSection
                "System"
                [ omniMenuItem "Lock" "system-lock-screen" "loginctl lock-session",
                  omniMenuItem "Toggle screensaver" "video-display" "hypr-screensaver toggle",
                  omniMenuItem "Reload WM" "view-refresh" "sh -lc 'hyprctl reload || xmonad --restart || river-xmonad-restart'",
                  omniMenuItem "Restart taffybar" "view-refresh-symbolic" "/srv/dotfiles/dotfiles/config/taffybar/scripts/taffybar-restart",
                  omniMenuItem "Logout" "system-log-out" "sh -lc 'hyprctl dispatch exit || riverctl exit'",
                  omniMenuItem "Suspend" "media-playback-pause" "systemctl suspend",
                  omniMenuItem "Reboot" "system-reboot" "systemctl reboot",
                  omniMenuItem "Power off" "system-shutdown" "systemctl poweroff"
                ]
            ]
        }

usageSectionWidget :: Text -> FilePath -> Text -> TaffyIO Gtk.Widget -> TaffyIO Gtk.Widget
usageSectionWidget klass iconFile tooltip stackBuilder =
  decorateWithClassAndBoxM klass $ do
    stack <- stackBuilder
    liftIO $ do
      iconWidget <- usageLogoWidget iconFile tooltip
      section <- buildIconLabelBox iconWidget stack
      widgetSetClassGI section "usage-section"

sniPriorityVisibilityThresholdDefault :: Int
sniPriorityVisibilityThresholdDefault = 0

localSendTrayMatcher :: SNITray.TrayItemMatcher
localSendTrayMatcher = SNITray.trayMatchIconTitleEquals "localsend_app"

localSendTrayClickHook :: SNITray.TrayClickHook
localSendTrayClickHook clickContext
  | SNITray.trayClickButton clickContext == 1,
    SNITray.trayItemMatcherPredicate
      localSendTrayMatcher
      (SNITray.trayClickItemInfo clickContext) = do
      void $ forkIO $ callProcess "hyprctl" ["eval", "_G.im_hyprland_toggle_localsend_scratchpad()"]
      pure SNITray.ConsumeClick
  | otherwise = pure SNITray.UseDefaultClickAction

sniTrayWidget :: TaffyIO Gtk.Widget
sniTrayWidget = do
  -- If the Haskell backend regresses, flip at runtime:
  --   TAFFYBAR_SNI_MENU_BACKEND=lib
  backendEnv <- liftIO (lookupEnv "TAFFYBAR_SNI_MENU_BACKEND")
  thresholdEnv <- liftIO (lookupEnv "TAFFYBAR_SNI_PRIORITY_THRESHOLD")
  let menuBackend =
        case fmap (map toLower) backendEnv of
          Just "lib" -> SNITray.LibDBusMenu
          _ -> SNITray.HaskellDBusMenu
      visibilityThreshold =
        fromMaybe
          sniPriorityVisibilityThresholdDefault
          (thresholdEnv >>= readMaybe)
      trayParams =
        SNITray.defaultTrayParams
          { SNITray.trayMenuBackend = menuBackend,
            SNITray.trayOverlayScale = 1 % 3,
            SNITray.trayEventHooks =
              SNITray.defaultTrayEventHooks
                { SNITray.trayClickHook = Just localSendTrayClickHook
                }
          }
      sniTrayConfig =
        defaultSNITrayConfig
          { sniTrayTrayParams = trayParams
          }
      collapsibleParams =
        defaultCollapsibleSNITrayParams
          { collapsibleSNITrayConfig = sniTrayConfig
          }
      prioritizedParams =
        defaultPrioritizedCollapsibleSNITrayParams
          { prioritizedCollapsibleSNITrayParams = collapsibleParams,
            prioritizedCollapsibleSNITrayVisibilityThreshold = Just visibilityThreshold,
            prioritizedCollapsibleSNITrayHoverExpand = True
          }
  decorateWithClassAndBoxM
    "sni-tray"
    (sniTrayPrioritizedCollapsibleNewFromParams prioritizedParams)

startWidgetsForBackend :: Backend -> [TaffyIO Gtk.Widget]
startWidgetsForBackend backend =
  case backend of
    BackendX11 -> [omniMenuWidget, workspacesWidget, layoutWidget]
    -- These Wayland widgets are Hyprland-specific.
    BackendWayland -> [omniMenuWidget, workspacesWidget]

startWidgetsForHostAndBackend :: String -> Backend -> [TaffyIO Gtk.Widget]
startWidgetsForHostAndBackend _hostName = startWidgetsForBackend

endWidgetsForHost :: String -> [TaffyIO Gtk.Widget]
endWidgetsForHost hostName =
  -- NOTE: end widgets are packed with Gtk.boxPackEnd, so the list order is
  -- right-to-left on screen. Make the tray appear at the far right by placing
  -- it first in the list. (On laptops: the battery/wifi stack is far right,
  -- tray immediately left of it.)
  let hostCPUWidget = cpuWidget $ hostName `elem` cpuPowerOverlayHosts
      baseEndWidgets =
        [ sniTrayWidget,
          audioWidget,
          aiUsageWidget,
          hostCPUWidget,
          cpuGpuTemperatureWidget,
          ramSwapWidget,
          diskUsageWidget,
          networkWidget,
          sunLockWidget,
          mprisWidget
        ]
      laptopEndWidgets =
        [ batteryNetworkWidget,
          sniTrayWidget,
          diskCPUFrequencyWidget,
          audioBacklightWidget,
          aiUsageWidget,
          hostCPUWidget,
          cpuGpuTemperatureWidget,
          ramSwapWidget,
          sunLockWidget,
          mprisWidget
        ]
   in if hostName `elem` laptopHosts
        then laptopEndWidgets
        else baseEndWidgets
