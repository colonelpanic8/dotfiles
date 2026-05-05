{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.Widgets
  ( clockWidget,
    endWidgetsForHost,
    startWidgetsForBackend,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified StatusNotifier.Tray as SNITray
import TaffybarConfig.Host (laptopHosts)
import TaffybarConfig.WidgetUtil
  ( decorateWithClassAndBox,
    decorateWithClassAndBoxM,
    setFixedLabelWidth,
    setLabelAlignmentRecursively,
    stackInPill,
    usageLogoWidget,
  )
import TaffybarConfig.Workspaces (workspaceLabelSetter, workspaceWindowIconGetter)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Taffybar.Context
  ( Backend (BackendWayland, BackendX11),
    TaffyIO,
  )
import System.Taffybar.Information.Memory (MemoryInfo (..), parseMeminfo)
import qualified System.Taffybar.Information.Workspaces.Model as WorkspaceModel
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget
import qualified System.Taffybar.Widget.ASUS as ASUS
import System.Taffybar.Widget.AnthropicUsage
  ( AnthropicUsageDisplayMode (AnthropicUsageDisplayRemaining),
    AnthropicUsageStackConfig (..),
    anthropicUsageSectionNewWith,
    defaultAnthropicUsageStackConfig,
  )
import System.Taffybar.Widget.CPUMonitor (cpuMonitorNew)
import System.Taffybar.Widget.Generic.Graph (GraphConfig (..), GraphDirection (..), GraphStyle (..), defaultGraphConfig)
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import System.Taffybar.Widget.OpenAIUsage
  ( OpenAIUsageDisplayMode (OpenAIUsageDisplayRemaining),
    OpenAIUsageStackConfig (..),
    defaultOpenAIUsageStackConfig,
    openAIUsageSectionNewWith,
  )
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
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
  ( backgroundLoop,
    buildIconLabelBox,
    pixbufNewFromFileAtScaleByHeight,
    widgetSetClassGI,
  )
import qualified System.Taffybar.Widget.Wlsunset as Wlsunset
import qualified System.Taffybar.Widget.Workspaces as Workspaces
import Text.Printf (printf)
import Text.Read (readMaybe)

audioWidget :: TaffyIO Gtk.Widget
audioWidget =
  decorateWithClassAndBoxM "audio" PulseAudio.pulseAudioNew

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
          Workspaces.labelSetter = workspaceLabelSetter,
          Workspaces.showWorkspaceFn =
            \workspace ->
              Workspaces.hideEmpty workspace
                && not (WorkspaceModel.workspaceIsSpecial workspace)
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
  labelWidget <- textBatteryNew "$percentage$%"
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
meminfoPercentRowWidget rowClass iconText getRatio tooltipText =
  liftIO $ do
    iconW <- Gtk.toWidget =<< Gtk.labelNew (Just iconText)
    valueLabel <- Gtk.labelNew (Just "")
    valueW <- Gtk.toWidget valueLabel
    row <- buildIconLabelBox iconW valueW
    _ <- widgetSetClassGI row rowClass

    let fmtPercent :: Double -> T.Text
        fmtPercent r = T.pack (printf "%.0f%%" (max 0 r * 100))
        updateOnce :: IO ()
        updateOnce = do
          info <- parseMeminfo
          let valueText = maybe "n/a" fmtPercent (getRatio info)
          postGUIASync $ do
            Gtk.labelSetText valueLabel valueText
            Gtk.widgetSetTooltipText row (Just (tooltipText info))
          threadDelay (2 * 1000000)

    _ <- Gtk.onWidgetRealize row $ backgroundLoop updateOnce
    pure row

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
    [ PulseAudio.pulseAudioNew,
      backlightNewChanWith
        defaultBacklightWidgetConfig
          { backlightFormat = "$percent$%",
            backlightUnknownFormat = "n/a",
            backlightTooltipFormat =
              Just "Device: $device$\nBrightness: $brightness$/$max$ ($percent$%)"
          }
    ]

asusInnerWidget :: TaffyIO Gtk.Widget
asusInnerWidget = ASUS.asusWidgetNew

asusWidget :: TaffyIO Gtk.Widget
asusWidget =
  decorateWithClassAndBoxM "asus-profile" asusInnerWidget

batteryNetworkWidget :: TaffyIO Gtk.Widget
batteryNetworkWidget =
  stackInPill "battery-network" [batteryInnerWidget, networkInnerWidget]

asusDiskUsageWidget :: TaffyIO Gtk.Widget
asusDiskUsageWidget =
  stackInPill "asus-disk-usage" [diskUsageInnerWidget, asusInnerWidget]

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

cpuWidget :: TaffyIO Gtk.Widget
cpuWidget =
  decorateWithClassAndBoxM "cpu" $
    cpuMonitorNew
      defaultGraphConfig
        { graphDataColors = [(0, 1, 0.5, 0.8), (1, 0, 0, 0.5)],
          graphBackgroundColor = (0, 0, 0, 0),
          graphBorderWidth = 0,
          graphLabel = Just "CPU",
          graphWidth = 50,
          graphDirection = LEFT_TO_RIGHT
        }
      1.0
      "cpu"

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
                  omniMenuItem "Toggle screensaver" "video-display" "/home/imalison/dotfiles/dotfiles/lib/bin/hypr-screensaver toggle",
                  omniMenuItem "Reload WM" "view-refresh" "sh -lc 'hyprctl reload || xmonad --restart || river-xmonad-restart'",
                  omniMenuItem "Restart taffybar" "view-refresh-symbolic" "/home/imalison/dotfiles/dotfiles/config/taffybar/scripts/taffybar-restart",
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

openAIUsageWidget :: TaffyIO Gtk.Widget
openAIUsageWidget = do
  iconWidget <- liftIO $ usageLogoWidget "openai-symbol.svg" "OpenAI usage"
  decorateWithClassAndBoxM "openai-usage" $
    openAIUsageSectionNewWith
      iconWidget
      defaultOpenAIUsageStackConfig
        { openAIUsageStackDefaultDisplayMode = OpenAIUsageDisplayRemaining
        }

anthropicUsageWidget :: TaffyIO Gtk.Widget
anthropicUsageWidget = do
  iconWidget <- liftIO $ usageLogoWidget "claude-symbol.svg" "Anthropic usage"
  decorateWithClassAndBoxM "anthropic-usage" $
    anthropicUsageSectionNewWith
      iconWidget
      defaultAnthropicUsageStackConfig
        { anthropicUsageStackDefaultDisplayMode = AnthropicUsageDisplayRemaining
        }

sniPriorityVisibilityThresholdDefault :: Int
sniPriorityVisibilityThresholdDefault = 0

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
            SNITray.trayEventHooks = SNITray.defaultTrayEventHooks
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
            prioritizedCollapsibleSNITrayVisibilityThreshold = Just visibilityThreshold
          }
  decorateWithClassAndBoxM
    "sni-tray"
    (sniTrayPrioritizedCollapsibleNewFromParams prioritizedParams)

startWidgetsForBackend :: Backend -> [TaffyIO Gtk.Widget]
startWidgetsForBackend backend =
  case backend of
    BackendX11 -> [omniMenuWidget, workspacesWidget, layoutWidget, windowsWidget]
    -- These Wayland widgets are Hyprland-specific.
    BackendWayland -> [omniMenuWidget, workspacesWidget, windowsWidget]

endWidgetsForHost :: String -> [TaffyIO Gtk.Widget]
endWidgetsForHost hostName =
  -- NOTE: end widgets are packed with Gtk.boxPackEnd, so the list order is
  -- right-to-left on screen. Make the tray appear at the far right by placing
  -- it first in the list. (On laptops: the battery/wifi stack is far right,
  -- tray immediately left of it.)
  let baseEndWidgets =
        [ sniTrayWidget,
          audioWidget,
          anthropicUsageWidget,
          openAIUsageWidget,
          cpuWidget,
          ramSwapWidget,
          diskUsageWidget,
          networkWidget,
          sunLockWidget,
          mprisWidget
        ]
      laptopEndWidgets =
        [ batteryNetworkWidget,
          sniTrayWidget,
          asusDiskUsageWidget,
          audioBacklightWidget,
          anthropicUsageWidget,
          openAIUsageWidget,
          cpuWidget,
          ramSwapWidget,
          sunLockWidget,
          mprisWidget
        ]
   in if hostName `elem` laptopHosts
        then laptopEndWidgets
        else baseEndWidgets
