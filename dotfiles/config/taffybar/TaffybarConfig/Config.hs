module TaffybarConfig.Config
  ( mkTaffybarConfig,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import System.Taffybar.Context (Backend, TaffybarConfig)
import System.Taffybar.SimpleConfig
import TaffybarConfig.Profile
import TaffybarConfig.RuntimeCapabilities
import TaffybarConfig.Widgets
  ( clockWidget,
    endWidgetsForCapabilities,
    startWidgetsForBackend,
  )

data MonitorGeometry = MonitorGeometry
  { monitorLogicalWidth :: Int,
    monitorLogicalHeight :: Int,
    monitorScale :: Int
  }

getMonitorGeometry :: Int -> IO (Maybe MonitorGeometry)
getMonitorGeometry monitorNumber = do
  maybeDisplay <- Gdk.displayGetDefault
  case maybeDisplay of
    Nothing -> pure Nothing
    Just display -> do
      maybeMonitor <- Gdk.displayGetMonitor display $ fromIntegral monitorNumber
      case maybeMonitor of
        Nothing -> pure Nothing
        Just monitor -> do
          geometry <- Gdk.monitorGetGeometry monitor
          width <- fromIntegral <$> Gdk.getRectangleWidth geometry
          height <- fromIntegral <$> Gdk.getRectangleHeight geometry
          scale <- fromIntegral <$> Gdk.monitorGetScaleFactor monitor
          pure $
            Just $
              MonitorGeometry
                { monitorLogicalWidth = width,
                  monitorLogicalHeight = height,
                  monitorScale = scale
                }

monitorConfigKey :: MonitorGeometry -> BarProfile -> RuntimeCapabilities -> T.Text
monitorConfigKey geometry profile capabilities =
  T.intercalate
    (T.pack ":")
    [ barProfileClass profile,
      T.pack $ show $ monitorLogicalWidth geometry,
      T.pack $ show $ monitorLogicalHeight geometry,
      T.pack $ show $ monitorScale geometry,
      T.pack $ show capabilities
    ]

mkTaffybarConfig :: Backend -> [FilePath] -> TaffybarConfig
mkTaffybarConfig backend cssFiles =
  toTaffybarConfigPerMonitor baseConfig $ \monitorNumber -> do
    capabilities <- getRuntimeCapabilities
    maybeGeometry <- liftIO $ getMonitorGeometry monitorNumber
    let geometry =
          maybe
            (MonitorGeometry 3840 2160 1)
            id
            maybeGeometry
        profile = profileForLogicalWidth $ monitorLogicalWidth geometry
        config =
          baseConfig
            { barCssClasses = [barProfileClass profile],
              endWidgets = endWidgetsForCapabilities capabilities,
              barPadding = barProfilePadding profile,
              barHeight = ScreenRatio $ barProfileHeightRatio profile
            }
    pure $
      SimpleMonitorConfig
        { simpleMonitorConfigKey = monitorConfigKey geometry profile capabilities,
          simpleMonitorConfig = config
        }
  where
    baseConfig =
      defaultSimpleTaffyConfig
        { startWidgets = startWidgetsForBackend backend,
          centerWidgets = [clockWidget],
          endWidgets = [],
          barLevels = Nothing,
          barPosition = Top,
          widgetSpacing = 0,
          cssPaths = cssFiles,
          startupHook = startRuntimeCapabilityMonitoring
        }
