{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.List (nub, sortOn, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import Network.HostName (getHostName)
import qualified StatusNotifier.Host.Service as SNIHost
import qualified StatusNotifier.Tray as SNITray
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (takeDirectory)
import System.Log.Logger (Priority (WARNING), rootLoggerName, setLevel, updateGlobalLogger)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( Backend (BackendWayland, BackendX11),
    TaffyIO,
    backend,
    detectBackend,
    runX11Def,
  )
import System.Taffybar.DBus
import System.Taffybar.DBus.Toggle
import System.Taffybar.Hooks (withLogLevels)
import System.Taffybar.Information.EWMHDesktopInfo (WorkspaceId (..))
import qualified System.Taffybar.Information.Workspaces.Model as WorkspaceModel
import System.Taffybar.Information.Memory (MemoryInfo (..), parseMeminfo)
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.SimpleConfig
import System.Taffybar.Util (getPixbufFromFilePath, maybeTCombine, postGUIASync, (<|||>))
import System.Taffybar.Widget
import qualified System.Taffybar.Widget.ASUS as ASUS
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
import System.Taffybar.Widget.SNIMenu (withNmAppletMenu)
import System.Taffybar.Widget.SNITray
  ( SNITrayConfig (..),
    defaultSNITrayConfig,
    sniTrayNewFromConfig,
    sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt,
  )
import qualified System.Taffybar.Widget.ScreenLock as ScreenLock
import System.Taffybar.Widget.Util (backgroundLoop, buildContentsBox, buildIconLabelBox, loadPixbufByName, widgetSetClassGI)
import qualified System.Taffybar.Widget.Wlsunset as Wlsunset
import qualified System.Taffybar.Widget.Workspaces.Hyprland as Hyprland
import qualified System.Taffybar.Widget.Workspaces as Workspaces
import System.Taffybar.WindowIcon (pixBufFromColor)
import Text.Printf (printf)
import Text.Read (readMaybe)

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

remapNSP :: String -> String
remapNSP "NSP" = "S"
remapNSP n = n

workspaceLabelSetter :: WorkspaceModel.WorkspaceInfo -> TaffyIO String
workspaceLabelSetter workspace = do
  backendType <- asks backend
  let identity = WorkspaceModel.workspaceIdentity workspace
      fallbackLabel = remapNSP $ T.unpack (WorkspaceModel.workspaceName identity)
  case (backendType, WorkspaceModel.workspaceNumericId identity) of
    (BackendX11, Just workspaceId) -> do
      fullNames <- runX11Def [] x11FullWorkspaceNames
      return $ remapNSP $ fromMaybe fallbackLabel (lookup (WorkspaceId workspaceId) fullNames)
    _ -> return fallbackLabel

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

workspaceIconCandidates :: WorkspaceModel.WindowInfo -> [Text]
workspaceIconCandidates windowData =
  let baseNames = WorkspaceModel.windowClassHints windowData
      remapped = concatMap lookupIconRemap baseNames
      remappedExpanded = concatMap iconNameVariants remapped
      baseExpanded = concatMap iconNameVariants baseNames
   in nub (remappedExpanded ++ baseExpanded)

isPathCandidate :: Text -> Bool
isPathCandidate name =
  T.isInfixOf "/" name
    || any (`T.isSuffixOf` name) [".png", ".svg", ".xpm"]

workspaceIconFromCandidate :: Int32 -> Text -> TaffyIO (Maybe Gdk.Pixbuf)
workspaceIconFromCandidate size name
  | isPathCandidate name =
      liftIO $ getPixbufFromFilePath (T.unpack name)
  | otherwise =
      maybeTCombine
        (Hyprland.getWindowIconFromDesktopEntryByAppId size (T.unpack name))
        (liftIO $ loadPixbufByName size name)

workspaceManualIconGetter :: Workspaces.WindowIconPixbufGetter
workspaceManualIconGetter =
  Workspaces.handleIconGetterException $ \size windowData ->
    foldl maybeTCombine (return Nothing) $
      map (workspaceIconFromCandidate size) (workspaceIconCandidates windowData)

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

workspaceFallbackIcon :: Workspaces.WindowIconPixbufGetter
workspaceFallbackIcon size _ =
  fallbackIconPixbuf size

workspaceWindowIconGetter :: Workspaces.WindowIconPixbufGetter
workspaceWindowIconGetter =
  workspaceManualIconGetter
    <|||> Workspaces.getWindowIconPixbufFromChrome
    <|||> Workspaces.defaultGetWindowIconPixbuf
    <|||> workspaceFallbackIcon

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
  decorateWithClassAndBoxM "windows" (windowsNew defaultWindowsConfig)

workspacesWidget :: TaffyIO Gtk.Widget
workspacesWidget = Workspaces.workspacesNew cfg
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
clockWidget =
  decorateWithClassAndBoxM
    "clock"
    ( textClockNewWith
        defaultClockConfig
          { clockUpdateStrategy = RoundedTargetInterval 60 0.0,
            clockFormatString = "%a %b %_d\n%I:%M %p"
          }
    )

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
                  \np -> stackedMprisLabel <$> playingText 20 20 np
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

wakeupDebugWidget :: TaffyIO Gtk.Widget
wakeupDebugWidget =
  decorateWithClassAndBoxM "wakeup-debug" wakeupDebugWidgetNew

type SNIPriorityMap = M.Map String Int

sniPriorityStateRelativePath :: FilePath
sniPriorityStateRelativePath = "sni-priorities.dat"

sniPriorityMin, sniPriorityMax, sniCollapsedPriorityThreshold :: Int
sniPriorityMin = -5
sniPriorityMax = 5
sniCollapsedPriorityThreshold = 0

clampSNIPriority :: Int -> Int
clampSNIPriority =
  max sniPriorityMin . min sniPriorityMax

sniPriorityStatePath :: IO FilePath
sniPriorityStatePath = getUserConfigFile "taffybar" sniPriorityStateRelativePath

loadSNIPriorityMap :: IO SNIPriorityMap
loadSNIPriorityMap = do
  path <- sniPriorityStatePath
  exists <- doesFileExist path
  if not exists
    then return M.empty
    else do
      content <- readFile path
      return $ fromMaybe M.empty $ M.fromList <$> readMaybe content

persistSNIPriorityMap :: SNIPriorityMap -> IO ()
persistSNIPriorityMap priorities = do
  path <- sniPriorityStatePath
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (show (M.toList priorities))

nonEmptyString :: String -> Maybe String
nonEmptyString value
  | null value = Nothing
  | otherwise = Just value

priorityKeyCandidates :: SNIHost.ItemInfo -> [String]
priorityKeyCandidates info =
  concat
    [ map ("item-id:" ++) (maybeToList (SNIHost.itemId info >>= nonEmptyString)),
      map ("icon-name:" ++) (maybeToList (nonEmptyString (SNIHost.iconName info))),
      map ("icon-title:" ++) (maybeToList (nonEmptyString (SNIHost.iconTitle info)))
    ]

priorityKeyFromItem :: SNIHost.ItemInfo -> Maybe String
priorityKeyFromItem = listToMaybe . priorityKeyCandidates

itemMatchesPriorityKey :: String -> SNIHost.ItemInfo -> Bool
itemMatchesPriorityKey key info =
  case stripPrefix "item-id:" key of
    Just itemIdKey -> SNIHost.itemId info == Just itemIdKey
    Nothing ->
      case stripPrefix "icon-name:" key of
        Just iconNameKey -> SNIHost.iconName info == iconNameKey
        Nothing ->
          case stripPrefix "icon-title:" key of
            Just iconTitleKey -> SNIHost.iconTitle info == iconTitleKey
            Nothing -> False

priorityMatchersFromMap :: SNIPriorityMap -> [SNITray.TrayItemMatcher]
priorityMatchersFromMap priorities =
  let sortedEntries = sortOn snd $ M.toList priorities
      entryMatcher (key, priority) =
        SNITray.mkTrayItemMatcher
          (printf "priority:%d:%s" priority key)
          (itemMatchesPriorityKey key)
      fallbackMatcher =
        SNITray.mkTrayItemMatcher "priority:fallback" (const True)
   in
    if null sortedEntries
      then []
      else map entryMatcher sortedEntries ++ [fallbackMatcher]

collapsedPriorityCutoffFromMap :: SNIPriorityMap -> Int
collapsedPriorityCutoffFromMap priorities =
  let sortedEntries = sortOn snd $ M.toList priorities
      visibleIndexes =
        [ idx
        | (idx, (_, priority)) <- zip [0 :: Int ..] sortedEntries,
          priority <= sniCollapsedPriorityThreshold
        ]
   in fromMaybe (-1) (safeLast visibleIndexes)
  where
    safeLast [] = Nothing
    safeLast xs = Just (last xs)

hasPriorityGestureModifiers :: [Gdk.ModifierType] -> Bool
hasPriorityGestureModifiers modifiers =
  Gdk.ModifierTypeControlMask `elem` modifiers
    && Gdk.ModifierTypeShiftMask `elem` modifiers

modifyPriorityMap ::
  IORef SNIPriorityMap ->
  String ->
  (Maybe Int -> Maybe Int) ->
  IO ()
modifyPriorityMap prioritiesRef key editPriority = do
  modifyIORef' prioritiesRef (M.alter editPriority key)
  readIORef prioritiesRef >>= persistSNIPriorityMap

priorityClickHookFromRef :: IORef SNIPriorityMap -> SNITray.TrayClickHook
priorityClickHookFromRef prioritiesRef clickContext = do
  let modifiers = SNITray.trayClickModifiers clickContext
      button = SNITray.trayClickButton clickContext
      clickedInfo = SNITray.trayClickItemInfo clickContext
      updatePriority delta =
        Just . clampSNIPriority . maybe 0 (+ delta)
  case (hasPriorityGestureModifiers modifiers, priorityKeyFromItem clickedInfo) of
    (False, _) -> return SNITray.UseDefaultClickAction
    (_, Nothing) -> return SNITray.UseDefaultClickAction
    (True, Just key) -> do
      case button of
        1 ->
          modifyPriorityMap prioritiesRef key (updatePriority (-1))
        2 ->
          modifyPriorityMap prioritiesRef key (const Nothing)
        3 ->
          modifyPriorityMap prioritiesRef key (updatePriority 1)
        _ ->
          return ()
      return SNITray.ConsumeClick

sniTrayWidget :: TaffyIO Gtk.Widget
sniTrayWidget = do
  -- If the Haskell backend regresses, flip at runtime:
  --   TAFFYBAR_SNI_MENU_BACKEND=lib
  backendEnv <- liftIO (lookupEnv "TAFFYBAR_SNI_MENU_BACKEND")
  priorityMap <- liftIO loadSNIPriorityMap
  priorityMapRef <- liftIO (newIORef priorityMap)
  let menuBackend =
        case fmap (map toLower) backendEnv of
          Just "lib" -> SNITray.LibDBusMenu
          _ -> SNITray.HaskellDBusMenu
      trayEventHooks =
        SNITray.defaultTrayEventHooks
          { SNITray.trayClickHook = Just (priorityClickHookFromRef priorityMapRef)
          }
      trayParams =
        SNITray.defaultTrayParams
          { SNITray.trayMenuBackend = menuBackend,
            SNITray.trayOverlayScale = 1 % 3,
            SNITray.trayEventHooks = trayEventHooks
          }
      trayPriorityConfig =
        SNITray.defaultTrayPriorityConfig
          { SNITray.trayPriorityMatchers = priorityMatchersFromMap priorityMap
          }
      sniTrayConfig =
        defaultSNITrayConfig
          { sniTrayTrayParams = trayParams,
            sniTrayPriorityConfig = trayPriorityConfig
          }
  decorateWithClassAndBoxM
    "sni-tray"
    (sniTrayNewFromConfig sniTrayConfig)

-- ** Layout

startWidgetsForBackend :: Backend -> [TaffyIO Gtk.Widget]
startWidgetsForBackend backend =
  case backend of
    BackendX11 -> [workspacesWidget, layoutWidget, windowsWidget]
    -- These Wayland widgets are Hyprland-specific.
    BackendWayland -> [workspacesWidget, windowsWidget]

endWidgetsForHost :: String -> [TaffyIO Gtk.Widget]
endWidgetsForHost hostName =
  -- NOTE: end widgets are packed with Gtk.boxPackEnd, so the list order is
  -- right-to-left on screen. Make the tray appear at the far right by placing
  -- it first in the list. (On laptops: the battery/wifi stack is far right,
  -- tray immediately left of it.)
  let baseEndWidgets =
        [ sniTrayWidget,
          audioWidget,
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
          ramSwapWidget,
          sunLockWidget,
          mprisWidget
        ]
   in if hostName `elem` laptopHosts
        then laptopEndWidgets
        else baseEndWidgets

mkSimpleTaffyConfig :: String -> Backend -> [FilePath] -> SimpleTaffyConfig
mkSimpleTaffyConfig hostName backend cssFiles =
  defaultSimpleTaffyConfig
    { startWidgets = startWidgetsForBackend backend,
      centerWidgets = [clockWidget],
      endWidgets = endWidgetsForHost hostName,
      barLevels = Nothing,
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
