{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toLower)
import Data.Int (Int32)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import Network.HostName (getHostName)
import System.Environment (lookupEnv)
import qualified StatusNotifier.Tray as SNITray
  ( MenuBackend (HaskellDBusMenu, LibDBusMenu),
    defaultTrayParams,
    trayMenuBackend,
    trayOverlayScale,
  )
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Log.Logger (Priority (WARNING), rootLoggerName, setLevel, updateGlobalLogger)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (Backend (BackendWayland, BackendX11), TaffyIO, detectBackend)
import System.Taffybar.DBus
import System.Taffybar.DBus.Toggle
import System.Taffybar.Hooks (withLogLevels)
import System.Taffybar.Information.Memory (MemoryInfo (..), parseMeminfo)
import System.Taffybar.Information.EWMHDesktopInfo (WorkspaceId (..))
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.SimpleConfig
import System.Taffybar.Util (getPixbufFromFilePath, maybeTCombine, postGUIASync, (<|||>))
import System.Taffybar.Widget
import qualified System.Taffybar.Widget.ASUS as ASUS
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
import System.Taffybar.Widget.SNIMenu (withNmAppletMenu)
import System.Taffybar.Widget.SNITray
  ( sniTrayNewFromParams,
    sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt,
  )
import qualified System.Taffybar.Widget.ScreenLock as ScreenLock
import System.Taffybar.Widget.Util (backgroundLoop, buildContentsBox, buildIconLabelBox, loadPixbufByName, widgetSetClassGI)
import qualified System.Taffybar.Widget.Wlsunset as Wlsunset
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceWidgetConfig
import qualified System.Taffybar.Widget.Workspaces.EWMH as X11Workspaces
import qualified System.Taffybar.Widget.Workspaces.Hyprland as Hyprland
import System.Taffybar.WindowIcon (pixBufFromColor)
import Text.Printf (printf)

-- | Wrap the widget in a "TaffyBox" (via 'buildContentsBox') and add a CSS class.
decorateWithClassAndBox :: (MonadIO m) => Text -> Gtk.Widget -> m Gtk.Widget
decorateWithClassAndBox klass widget = do
  boxed <- buildContentsBox widget
  widgetSetClassGI boxed klass

decorateWithClassAndBoxM :: (MonadIO m) => Text -> m Gtk.Widget -> m Gtk.Widget
decorateWithClassAndBoxM klass builder =
  builder >>= decorateWithClassAndBox klass

-- ** X11 Workspaces

x11FullWorkspaceNames :: X11Property [(WorkspaceId, String)]
x11FullWorkspaceNames =
  go <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
  where
    go = zip [WorkspaceId i | i <- [0 ..]]

x11WorkspaceLabelSetter :: X11Workspaces.Workspace -> X11Workspaces.WorkspacesIO String
x11WorkspaceLabelSetter workspace =
  remapNSP . fromMaybe "" . lookup (X11Workspaces.workspaceIdx workspace)
    <$> liftX11Def [] x11FullWorkspaceNames
  where
    remapNSP "NSP" = "S"
    remapNSP n = n

-- ** Logging

-- ** Hyprland Icon Finding

iconRemap :: [(Text, [Text])]
iconRemap =
  [ ("spotify", ["spotify-client", "spotify"])
  ]

iconRemapMap :: M.Map Text [Text]
iconRemapMap =
  M.fromList [(T.toLower k, v) | (k, v) <- iconRemap]

lookupIconRemap :: Text -> [Text]
lookupIconRemap name = fromMaybe [] $ M.lookup (T.toLower name) iconRemapMap

iconNameVariants :: Text -> [Text]
iconNameVariants raw =
  let lower = T.toLower raw
      stripped = fromMaybe lower (T.stripSuffix ".desktop" lower)
      suffixes = ["-gtk", "-client", "-desktop"]
      stripSuffixes name =
        let variants = mapMaybe (`T.stripSuffix` name) suffixes
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
              case T.splitOn "." name of
                [] -> name
                xs -> last xs
            dashed = T.map toDash name
            dashedDotted = T.map toDash dotted
            underscored = T.map toUnderscore name
            underscoredDotted = T.map toUnderscore dotted
         in [dotted, dashed, dashedDotted, underscored, underscoredDotted, name]
   in nub $ concatMap variantsFor baseNames

-- Hyprland "special" workspaces (e.g. "special:slack") are scratchpad-like and
-- usually not something we want visible in the workspace widget.
isSpecialHyprWorkspace :: Hyprland.HyprlandWorkspace -> Bool
isSpecialHyprWorkspace ws =
  let name = T.toLower $ T.pack $ Hyprland.workspaceName ws
   in T.isPrefixOf "special" name || Hyprland.workspaceIdx ws < 0

hyprlandIconCandidates :: Hyprland.HyprlandWindow -> [Text]
hyprlandIconCandidates windowData =
  let baseNames =
        map T.pack $
          catMaybes
            [ Hyprland.windowClass windowData,
              Hyprland.windowInitialClass windowData
            ]
      remapped = concatMap lookupIconRemap baseNames
      remappedExpanded = concatMap iconNameVariants remapped
      baseExpanded = concatMap iconNameVariants baseNames
   in nub (remappedExpanded ++ baseExpanded)

isPathCandidate :: Text -> Bool
isPathCandidate name =
  T.isInfixOf "/" name
    || any (`T.isSuffixOf` name) [".png", ".svg", ".xpm"]

hyprlandIconFromCandidate :: Int32 -> Text -> TaffyIO (Maybe Gdk.Pixbuf)
hyprlandIconFromCandidate size name
  | isPathCandidate name =
      liftIO $ getPixbufFromFilePath (T.unpack name)
  | otherwise =
      maybeTCombine
        (Hyprland.getWindowIconFromDesktopEntryByAppId size (T.unpack name))
        (liftIO $ loadPixbufByName size name)

hyprlandManualIconGetter :: Hyprland.HyprlandWindowIconPixbufGetter
hyprlandManualIconGetter =
  Hyprland.handleIconGetterException $ \size windowData ->
    foldl maybeTCombine (return Nothing) $
      map (hyprlandIconFromCandidate size) (hyprlandIconCandidates windowData)

fallbackIconPixbuf :: Int32 -> TaffyIO (Maybe Gdk.Pixbuf)
fallbackIconPixbuf size = do
  let fallbackNames =
        [ "application-x-executable",
          "application",
          "image-missing",
          "gtk-missing-image",
          "dialog-question",
          "utilities-terminal",
          "system-run",
          "window"
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

-- ** Host Overrides

defaultCssFiles :: [FilePath]
defaultCssFiles = ["palette.css", "taffybar.css"]

cssFilesByHostname :: [(String, [FilePath])]
cssFilesByHostname =
  [ ("imalison-home", ["palette.css", "taffybar.css"]),
    ("ryzen-shine", ["palette.css", "taffybar.css"]),
    ("stevie-nixos", ["palette.css", "taffybar.css"])
  ]

laptopHosts :: [String]
laptopHosts =
  [ "adell",
    "stevie-nixos",
    "strixi-minaj",
    "jay-lenovo"
  ]

cssFilesForHost :: String -> [FilePath]
cssFilesForHost hostName =
  fromMaybe defaultCssFiles $ lookup hostName cssFilesByHostname

-- ** Widgets

audioWidget :: TaffyIO Gtk.Widget
audioWidget =
  decorateWithClassAndBoxM "audio" PulseAudio.pulseAudioNew

networkWidget :: TaffyIO Gtk.Widget
networkWidget =
  decorateWithClassAndBoxM "network" $
    withNmAppletMenu NetworkManager.networkManagerWifiIconLabelNew

layoutWidget :: TaffyIO Gtk.Widget
layoutWidget =
  decorateWithClassAndBoxM "layout" (layoutNew defaultLayoutConfig)

windowsWidget :: TaffyIO Gtk.Widget
windowsWidget =
  decorateWithClassAndBoxM "windows" (windowsNew defaultWindowsConfig)

x11WorkspacesWidget :: TaffyIO Gtk.Widget
x11WorkspacesWidget =
  flip widgetSetClassGI "workspaces"
    =<< X11Workspaces.workspacesNew cfg
  where
    common =
      (X11Workspaces.workspacesCommonConfig X11Workspaces.defaultWorkspacesConfig)
        { WorkspaceWidgetConfig.minIcons = 1,
          WorkspaceWidgetConfig.getWindowIconPixbuf =
            X11Workspaces.scaledWindowIconPixbufGetter $
              X11Workspaces.getWindowIconPixbufFromChrome
                <|||> X11Workspaces.unscaledDefaultGetWindowIconPixbuf
                <|||> (\size _ -> fallbackIconPixbuf size),
          WorkspaceWidgetConfig.widgetGap = 0,
          WorkspaceWidgetConfig.showWorkspaceFn = X11Workspaces.hideEmpty,
          WorkspaceWidgetConfig.labelSetter = x11WorkspaceLabelSetter,
          WorkspaceWidgetConfig.widgetBuilder = X11Workspaces.buildLabelOverlayController
        }
    cfg =
      (X11Workspaces.applyCommonWorkspacesConfig common X11Workspaces.defaultWorkspacesConfig)
        { X11Workspaces.updateRateLimitMicroseconds = 100000
        }

-- | Like 'buildWorkspaceIconLabelOverlay' but lets you choose the corner.
buildAlignedOverlay ::
  Gtk.Align -> Gtk.Align -> Gtk.Widget -> Gtk.Widget -> TaffyIO Gtk.Widget
buildAlignedOverlay halign valign iconsWidget labelWidget = liftIO $ do
  base <- buildContentsBox iconsWidget
  ebox <- Gtk.eventBoxNew
  _ <- widgetSetClassGI ebox "overlay-box"
  Gtk.widgetSetHalign ebox halign
  Gtk.widgetSetValign ebox valign
  Gtk.containerAdd ebox labelWidget
  overlayLabel <- Gtk.toWidget ebox
  overlay <- Gtk.overlayNew
  baseW <- Gtk.toWidget base
  Gtk.containerAdd overlay baseW
  Gtk.overlayAddOverlay overlay overlayLabel
  Gtk.overlaySetOverlayPassThrough overlay overlayLabel True
  Gtk.toWidget overlay

hyprlandWorkspacesWidget :: TaffyIO Gtk.Widget
hyprlandWorkspacesWidget =
  flip widgetSetClassGI "workspaces"
    =<< Hyprland.hyprlandWorkspacesNew cfg
  where
    base = Hyprland.defaultHyprlandWorkspacesConfig
    cfg =
      Hyprland.applyCommonHyprlandWorkspacesConfig common base
    common =
      (Hyprland.hyprlandWorkspacesCommonConfig base)
        { WorkspaceWidgetConfig.widgetGap = 0,
          WorkspaceWidgetConfig.minIcons = 1,
          WorkspaceWidgetConfig.widgetBuilder =
            Hyprland.hyprlandBuildButtonController
              cfg
              ( Hyprland.hyprlandBuildCustomOverlayController
                  (buildAlignedOverlay Gtk.AlignStart Gtk.AlignEnd)
                  cfg
              ),
          -- Don't show Hyprland "special:*" workspaces.
          WorkspaceWidgetConfig.showWorkspaceFn =
            \ws ->
              Hyprland.workspaceState ws /= X11Workspaces.Empty
                && not (isSpecialHyprWorkspace ws),
          WorkspaceWidgetConfig.getWindowIconPixbuf =
            hyprlandManualIconGetter
              <|||> Hyprland.defaultHyprlandGetWindowIconPixbuf
              <|||> hyprlandFallbackIcon
        }

clockWidget :: TaffyIO Gtk.Widget
clockWidget =
  decorateWithClassAndBoxM
    "clock"
    ( textClockNewWith
        defaultClockConfig
          { clockUpdateStrategy = RoundedTargetInterval 60 0.0,
            clockFormatString = "%a %b %_d, 🕑%I:%M %p"
          }
    )

mprisWidget :: TaffyIO Gtk.Widget
mprisWidget =
  mpris2NewWithConfig
    MPRIS2Config
      { mprisWidgetWrapper = decorateWithClassAndBox "mpris",
        updatePlayerWidget =
          simplePlayerWidget
            defaultPlayerConfig
              { setNowPlayingLabel =
                  -- Upstream `playingText` uses "artist - title"; replace the
                  -- separator with a newline for a more compact widget.
                  \np -> T.replace " - " "\n" <$> playingText 20 20 np
              }
      }

batteryWidget :: TaffyIO Gtk.Widget
batteryWidget = do
  iconWidget <- batteryTextIconNew
  labelWidget <- textBatteryNew "$percentage$%"
  decorateWithClassAndBox "battery" =<< liftIO (buildIconLabelBox iconWidget labelWidget)

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

diskUsageWidget :: TaffyIO Gtk.Widget
diskUsageWidget =
  decorateWithClassAndBoxM "disk-usage" diskUsageNew

stackInPill :: Text -> [TaffyIO Gtk.Widget] -> TaffyIO Gtk.Widget
stackInPill klass builders =
  decorateWithClassAndBoxM klass $ do
    widgets <- sequence builders
    liftIO $ do
      box <- Gtk.boxNew Gtk.OrientationVertical 0
      mapM_ (\w -> Gtk.boxPackStart box w False False 0) widgets
      Gtk.widgetShowAll box
      Gtk.toWidget box

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

asusWidget :: TaffyIO Gtk.Widget
asusWidget =
  decorateWithClassAndBoxM "asus-profile" ASUS.asusWidgetNew

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
      { ScreenLock.screenLockIcon = T.pack "\xF023"
      }

simplifiedWlsunsetWidget :: TaffyIO Gtk.Widget
simplifiedWlsunsetWidget =
  -- Inner widget: no extra pill wrapping (the combiner provides that).
  Wlsunset.wlsunsetNewWithConfig
    Wlsunset.defaultWlsunsetWidgetConfig
      { Wlsunset.wlsunsetWidgetIcon = T.pack "\xF0599"
      }

sunLockWidget :: TaffyIO Gtk.Widget
sunLockWidget =
  stackInPill "sun-lock" [simplifiedWlsunsetWidget, simplifiedScreenLockWidget]

sniTrayWidget :: TaffyIO Gtk.Widget
sniTrayWidget = do
  -- If the Haskell backend regresses, flip at runtime:
  --   TAFFYBAR_SNI_MENU_BACKEND=lib
  backendEnv <- liftIO (lookupEnv "TAFFYBAR_SNI_MENU_BACKEND")
  let menuBackend =
        case fmap (map toLower) backendEnv of
          Just "lib" -> SNITray.LibDBusMenu
          _ -> SNITray.HaskellDBusMenu
  decorateWithClassAndBoxM
    "sni-tray"
    (sniTrayNewFromParams (SNITray.defaultTrayParams {SNITray.trayMenuBackend = menuBackend, SNITray.trayOverlayScale = 1 % 3}))

-- ** Layout

startWidgetsForBackend :: Backend -> [TaffyIO Gtk.Widget]
startWidgetsForBackend backend =
  case backend of
    BackendX11 -> [x11WorkspacesWidget, layoutWidget, windowsWidget]
    -- These Wayland widgets are Hyprland-specific.
    BackendWayland -> [hyprlandWorkspacesWidget]

endWidgetsForHost :: String -> [TaffyIO Gtk.Widget]
endWidgetsForHost hostName =
  let baseEndWidgets = [audioWidget, ramSwapWidget, diskUsageWidget, networkWidget, sunLockWidget, mprisWidget]
      laptopEndWidgets =
        [ batteryWidget,
          asusWidget,
          audioBacklightWidget,
          ramSwapWidget,
          diskUsageWidget,
          networkWidget,
          sunLockWidget,
          mprisWidget
        ]
   in if hostName `elem` laptopHosts
        then laptopEndWidgets
        else baseEndWidgets

barLevelsForHost :: String -> Backend -> [BarLevelConfig]
barLevelsForHost hostName backend =
  [ BarLevelConfig
      { levelStartWidgets = startWidgetsForBackend backend,
        levelCenterWidgets = [clockWidget],
        levelEndWidgets = endWidgetsForHost hostName
      },
    BarLevelConfig
      { levelStartWidgets = [],
        levelCenterWidgets = [],
        levelEndWidgets = [sniTrayWidget]
      }
  ]

mkSimpleTaffyConfig :: String -> Backend -> [FilePath] -> SimpleTaffyConfig
mkSimpleTaffyConfig hostName backend cssFiles =
  defaultSimpleTaffyConfig
    { startWidgets = [],
      centerWidgets = [],
      endWidgets = [],
      barLevels = Just $ barLevelsForHost hostName backend,
      barPosition = Top,
      widgetSpacing = 0,
      barPadding = 4,
      barHeight = ScreenRatio $ 1 / 33,
      cssPaths = cssFiles
    }

-- ** Entry Point

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel WARNING)

  hostName <- getHostName
  backend <- detectBackend
  cssFiles <- mapM (getUserConfigFile "taffybar") (cssFilesForHost hostName)

  let simpleTaffyConfig = mkSimpleTaffyConfig hostName backend cssFiles
  startTaffybar $
    withLogServer $
      withLogLevels $
        withToggleServer $
          toTaffybarConfig simpleTaffyConfig
