{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int (Int32)
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           Network.HostName (getHostName)
import           System.Environment.XDG.BaseDir (getUserConfigFile)
import           System.Log.Logger (Priority(WARNING), rootLoggerName, setLevel, updateGlobalLogger)
import           System.Taffybar (startTaffybar)
import           System.Taffybar.Context (Backend (BackendWayland, BackendX11), TaffyIO, detectBackend)
import           System.Taffybar.DBus
import           System.Taffybar.DBus.Toggle
import           System.Taffybar.Hooks (withLogLevels)
import           System.Taffybar.Information.EWMHDesktopInfo (WorkspaceId (..))
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util (getPixbufFromFilePath, (<|||>), maybeTCombine)
import           System.Taffybar.Widget
import qualified System.Taffybar.Widget.HyprlandWorkspaces as Hyprland
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import           System.Taffybar.Widget.SNIMenu (withNmAppletMenu)
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
import           System.Taffybar.Widget.SNITray
                 ( sniTrayNew
                 , sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
                 )
import           System.Taffybar.Widget.Util (buildContentsBox, buildIconLabelBox, loadPixbufByName, widgetSetClassGI)
import qualified System.Taffybar.Widget.Workspaces as X11Workspaces
import           System.Taffybar.WindowIcon (pixBufFromColor)

-- | Wrap the widget in a "TaffyBox" (via 'buildContentsBox') and add a CSS class.
decorateWithClassAndBox :: MonadIO m => Text -> Gtk.Widget -> m Gtk.Widget
decorateWithClassAndBox klass widget = do
  boxed <- buildContentsBox widget
  widgetSetClassGI boxed klass

decorateWithClassAndBoxM :: MonadIO m => Text -> m Gtk.Widget -> m Gtk.Widget
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
  remapNSP . fromMaybe "" . lookup (X11Workspaces.workspaceIdx workspace) <$>
    liftX11Def [] x11FullWorkspaceNames
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
  M.fromList [ (T.toLower k, v) | (k, v) <- iconRemap ]

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
  let baseNames = map T.pack $ catMaybes
        [ Hyprland.windowClass windowData
        , Hyprland.windowInitialClass windowData
        ]
      remapped = concatMap lookupIconRemap baseNames
      remappedExpanded = concatMap iconNameVariants remapped
      baseExpanded = concatMap iconNameVariants baseNames
  in nub (remappedExpanded ++ baseExpanded)

isPathCandidate :: Text -> Bool
isPathCandidate name =
  T.isInfixOf "/" name ||
  any (`T.isSuffixOf` name) [".png", ".svg", ".xpm"]

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

-- ** Host Overrides

defaultCssFiles :: [FilePath]
defaultCssFiles = ["palette.css", "taffybar.css"]

cssFilesByHostname :: [(String, [FilePath])]
cssFilesByHostname =
  [ ("imalison-home", ["palette.css", "taffybar.css"])
  , ("ryzen-shine", ["palette.css", "taffybar.css"])
  , ("stevie-nixos", ["palette.css", "taffybar.css"])
  ]

laptopHosts :: [String]
laptopHosts =
  [ "adell"
  , "stevie-nixos"
  , "strixi-minaj"
  , "jay-lenovo"
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
  flip widgetSetClassGI "workspaces" =<<
    X11Workspaces.workspacesNew
      X11Workspaces.defaultWorkspacesConfig
        { X11Workspaces.minIcons = 1
        , X11Workspaces.getWindowIconPixbuf =
            X11Workspaces.scaledWindowIconPixbufGetter $
              X11Workspaces.getWindowIconPixbufFromChrome <|||>
              X11Workspaces.unscaledDefaultGetWindowIconPixbuf <|||>
              (\size _ -> fallbackIconPixbuf size)
        , X11Workspaces.widgetGap = 0
        , X11Workspaces.showWorkspaceFn = X11Workspaces.hideEmpty
        , X11Workspaces.updateRateLimitMicroseconds = 100000
        , X11Workspaces.labelSetter = x11WorkspaceLabelSetter
        , X11Workspaces.widgetBuilder = X11Workspaces.buildLabelOverlayController
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
  flip widgetSetClassGI "workspaces" =<<
    Hyprland.hyprlandWorkspacesNew cfg
  where
    cfg = Hyprland.defaultHyprlandWorkspacesConfig
      { Hyprland.widgetGap = 0
      , Hyprland.minIcons = 1
      , Hyprland.widgetBuilder =
          Hyprland.hyprlandBuildButtonController cfg
            (Hyprland.hyprlandBuildCustomOverlayController
              (buildAlignedOverlay Gtk.AlignStart Gtk.AlignEnd)
              cfg)
      -- Don't show Hyprland "special:*" workspaces.
      , Hyprland.showWorkspaceFn =
          \ws ->
            Hyprland.workspaceState ws /= X11Workspaces.Empty &&
            not (isSpecialHyprWorkspace ws)
      , Hyprland.getWindowIconPixbuf =
          hyprlandManualIconGetter <|||>
          Hyprland.defaultHyprlandGetWindowIconPixbuf <|||>
          hyprlandFallbackIcon
      }

clockWidget :: TaffyIO Gtk.Widget
clockWidget =
  decorateWithClassAndBoxM
    "clock"
    ( textClockNewWith
        defaultClockConfig
          { clockUpdateStrategy = RoundedTargetInterval 60 0.0
          , clockFormatString = "%a %b %_d, 🕑%I:%M %p"
          }
    )

mprisWidget :: TaffyIO Gtk.Widget
mprisWidget =
  mpris2NewWithConfig
    MPRIS2Config
      { mprisWidgetWrapper = decorateWithClassAndBox "mpris"
      , updatePlayerWidget =
          simplePlayerWidget
            defaultPlayerConfig
              { setNowPlayingLabel = playingText 20 20
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
          { backlightFormat = "☀ $percent$%"
          , backlightUnknownFormat = "☀ n/a"
          , backlightTooltipFormat =
              Just "Device: $device$\nBrightness: $brightness$/$max$ ($percent$%)"
          }
    )

diskUsageWidget :: TaffyIO Gtk.Widget
diskUsageWidget =
  decorateWithClassAndBoxM "disk-usage" diskUsageNew

sniTrayWidget :: TaffyIO Gtk.Widget
sniTrayWidget =
  decorateWithClassAndBoxM
    "sni-tray"
    sniTrayNew

-- ** Layout

startWidgetsForBackend :: Backend -> [TaffyIO Gtk.Widget]
startWidgetsForBackend backend =
  case backend of
    BackendX11 -> [x11WorkspacesWidget, layoutWidget, windowsWidget]
    -- These Wayland widgets are Hyprland-specific.
    BackendWayland -> [hyprlandWorkspacesWidget]

endWidgetsForHost :: String -> Backend -> [TaffyIO Gtk.Widget]
endWidgetsForHost hostName backend =
  let tray = sniTrayWidget
      baseEndWidgets = [clockWidget, audioWidget, diskUsageWidget, networkWidget, mprisWidget]
      laptopEndWidgets =
          [ batteryWidget
          , clockWidget
          , audioWidget
          , diskUsageWidget
          , backlightWidget
          , networkWidget
          , mprisWidget
          ]
  in if hostName `elem` laptopHosts
    then laptopEndWidgets
    else baseEndWidgets

mkSimpleTaffyConfig :: String -> Backend -> [FilePath] -> SimpleTaffyConfig
mkSimpleTaffyConfig hostName backend cssFiles =
  defaultSimpleTaffyConfig
    { startWidgets = startWidgetsForBackend backend
    , endWidgets = endWidgetsForHost hostName backend
    , barPosition = Top
    , widgetSpacing = 0
    , barPadding = 4
    , barHeight = ScreenRatio $ 1 / 33
    , cssPaths = cssFiles
    , centerWidgets = [sniTrayWidget]
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
