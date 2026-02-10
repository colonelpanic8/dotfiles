{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ask)
import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (readMVar)
import           Control.Monad (void)
import           Data.Int (Int32)
import           Data.IORef (newIORef, readIORef, writeIORef)
import           Data.List (find, isInfixOf, nub)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           DBus
import qualified DBus.Client as DBusClient
import           Data.GI.Base (castTo)
import qualified DBusMenu
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as GdkE
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import           Network.HostName (getHostName)
import           System.Environment (lookupEnv)
import           System.Environment.XDG.BaseDir (getUserConfigFile)
import           System.Log.Logger (Priority(WARNING), logM, rootLoggerName, setLevel, updateGlobalLogger)
import           System.Taffybar (startTaffybar)
import           System.Taffybar.Context (Backend (BackendWayland, BackendX11), Context(..), TaffyIO, detectBackend)
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
import           Data.Ratio ((%))
import           System.Taffybar.Widget.SNITray
                 ( sniTrayNewFromParams
                 , sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
                 )
import qualified StatusNotifier.Tray as SNITray (MenuBackend (HaskellDBusMenu), defaultTrayParams, trayMenuBackend, trayOverlayScale)
import           System.Taffybar.Widget.Util (buildContentsBox, buildIconLabelBox, loadPixbufByName, widgetSetClassGI)
import qualified System.Taffybar.Widget.Workspaces as X11Workspaces
import           System.Taffybar.WindowIcon (pixBufFromColor)
import           Data.Ratio ((%))
import qualified StatusNotifier.Tray as SNITray (MenuBackend (HaskellDBusMenu), defaultTrayParams, trayMenuBackend, trayOverlayScale)

-- ** Debug: Programmatic SNI Menu Popup

-- | DBus Properties.Get helper.
propsGet :: DBusClient.Client -> BusName -> ObjectPath -> String -> String -> IO (Maybe Variant)
propsGet client dest obj iface prop = do
  let mc =
        (methodCall obj "org.freedesktop.DBus.Properties" "Get")
          { methodCallDestination = Just dest
          , methodCallBody = [toVariant iface, toVariant prop]
          }
  result <- DBusClient.call client mc
  case result of
    Left _ -> pure Nothing
    Right reply ->
      case methodReturnBody reply of
        [v] -> pure (fromVariant v)
        _ -> pure Nothing

-- | Return (bus name, object path, display string) for currently registered SNI
-- entries from the watcher.  Prefer this over RegisteredStatusNotifierItems,
-- which is only bus names and doesn't include object paths.
getRegisteredSNIEntries :: DBusClient.Client -> IO [(BusName, ObjectPath, String)]
getRegisteredSNIEntries client = do
  mv <- propsGet client watcherName watcherPath "org.kde.StatusNotifierWatcher" "RegisteredSNIEntries"
  let raw :: [(String, String)]
      raw = fromMaybe [] $ mv >>= fromVariant
  pure
    [ (busName_ bus, objectPath_ path, bus <> path)
    | (bus, path) <- raw
    ]
  where
    watcherName = busName_ "org.kde.StatusNotifierWatcher"
    watcherPath = objectPath_ "/StatusNotifierWatcher"

getSNIItemMenuPath :: DBusClient.Client -> BusName -> ObjectPath -> IO (Maybe ObjectPath)
getSNIItemMenuPath client itemBus itemPath = do
  mv <- propsGet client itemBus itemPath "org.kde.StatusNotifierItem" "Menu"
  pure $ mv >>= fromVariant

-- | Pop up the first submenu we can find under a menu.
popupFirstSubmenu :: Gtk.Menu -> IO ()
popupFirstSubmenu rootMenu = do
  children <- Gtk.containerGetChildren rootMenu
  let go [] = pure ()
      go (w:ws) = do
        mi <- castTo Gtk.MenuItem w
        case mi of
          Nothing -> go ws
          Just menuItem -> do
            smw <- Gtk.menuItemGetSubmenu menuItem
            case smw of
              Nothing -> go ws
              Just sw -> do
                sm <- castTo Gtk.Menu sw
                case sm of
                  Nothing -> go ws
                  Just submenu -> do
                    Gtk.widgetShowAll submenu
                    Gtk.menuPopupAtWidget
                      submenu
                      menuItem
                      GdkE.GravityNorthEast
                      GdkE.GravityNorthWest
                      Nothing
  go children

-- | When enabled by env vars, pop up an SNI menu (and a submenu if present) so
-- we can screenshot it in automation loops.
--
-- Env vars:
-- - TAFFYBAR_DEBUG_POPUP_SNI_MENU=1 to enable
-- - TAFFYBAR_DEBUG_SNI_MATCH=<substring> to choose an item (matches the raw item id)
debugPopupSNIMenuHook :: TaffyIO ()
debugPopupSNIMenuHook = do
  enabled <- liftIO $ lookupEnv "TAFFYBAR_DEBUG_POPUP_SNI_MENU"
  case enabled of
    Nothing -> pure ()
    Just _ -> do
      match <- liftIO $ fromMaybe "" <$> lookupEnv "TAFFYBAR_DEBUG_SNI_MATCH"
      ctx <- ask
      -- Poll until the tray watcher has registered items; on startup this can
      -- take a few seconds.
      liftIO $ do
        triesRef <- newIORef (40 :: Int) -- ~10s at 250ms
        void $ GLib.timeoutAdd GLib.PRIORITY_LOW 250 $ do
          let client = sessionDBusClient ctx
          entries <- getRegisteredSNIEntries client
          remaining <- readIORef triesRef
          if not (null entries)
            then do
              logM "TaffybarDebug" WARNING $
                "SNI debug popup: registered entries=" <> show (length entries)
              let chosen =
                    case match of
                      "" -> listToMaybe entries
                      _ -> find (\(_, _, disp) -> isInfixOf match disp) entries <|> listToMaybe entries
              case chosen of
                Nothing -> do
                  logM "TaffybarDebug" WARNING "SNI debug popup: no suitable item found."
                  pure False
                Just (itemBus, itemPath, disp) -> do
                  mMenuPath <- getSNIItemMenuPath client itemBus itemPath
                  case mMenuPath of
                    Nothing ->
                      do
                        logM "TaffybarDebug" WARNING $
                          "SNI debug popup: entry has no Menu property: " <> disp
                        pure False
                    Just menuPath -> do
                      logM "TaffybarDebug" WARNING $
                        "SNI debug popup: popping menu for " <> disp <> " menu=" <> show menuPath
                      gtkMenu <- DBusMenu.buildMenu client itemBus menuPath
                      -- Attach to the bar window if possible to keep CSS parent chain realistic.
                      wins <- readMVar (existingWindows ctx)
                      case wins of
                        ((_, win):_) -> Gtk.menuAttachToWidget gtkMenu win Nothing
                        _ -> pure ()
                      _ <- Gtk.onWidgetHide gtkMenu $
                        void $ GLib.idleAdd GLib.PRIORITY_LOW $ do
                          Gtk.widgetDestroy gtkMenu
                          pure False

                      Gtk.widgetShowAll gtkMenu
                      case wins of
                        ((_, win):_) ->
                          Gtk.menuPopupAtWidget
                            gtkMenu
                            win
                            GdkE.GravitySouthWest
                            GdkE.GravityNorthWest
                            Nothing
                        _ -> Gtk.menuPopupAtPointer gtkMenu Nothing

                      popupFirstSubmenu gtkMenu
                      pure False
          else if remaining <= 0
            then do
              logM "TaffybarDebug" WARNING "SNI debug popup: timed out waiting for tray items."
              pure False
            else do
              writeIORef triesRef (remaining - 1)
              pure True
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

hyprlandIconFromCandidate :: Int32 -> Text -> TaffyIO (Maybe GdkPixbuf.Pixbuf)
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

fallbackIconPixbuf :: Int32 -> TaffyIO (Maybe GdkPixbuf.Pixbuf)
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
    (sniTrayNewFromParams (SNITray.defaultTrayParams { SNITray.trayMenuBackend = SNITray.HaskellDBusMenu, SNITray.trayOverlayScale = 1 % 3 }))

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
    , startupHook = debugPopupSNIMenuHook
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
    withDebugServer $
    toTaffybarConfig simpleTaffyConfig
