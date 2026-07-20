{-# LANGUAGE OverloadedStrings #-}

module TaffybarConfig.Temperature
  ( cpuGpuTemperatureWidget,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Directory (findExecutable)
import System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Information.Temperature as TemperatureInfo
import qualified System.Taffybar.Widget.NvidiaTemperature as NvidiaTemperature
import qualified System.Taffybar.Widget.Temperature as Temperature
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import TaffybarConfig.WidgetUtil (stackInPill)

sensorNameContainsAny :: [T.Text] -> TemperatureInfo.ThermalSensor -> Bool
sensorNameContainsAny fragments sensor =
  let name = T.toCaseFold $ T.pack $ TemperatureInfo.sensorName sensor
   in any (`T.isInfixOf` name) fragments

cpuTemperatureConfig :: Temperature.TemperatureConfig
cpuTemperatureConfig =
  Temperature.defaultTemperatureConfig
    { Temperature.tempFormat = "CPU $temp$\176C",
      Temperature.tempSensorFilter =
        sensorNameContainsAny ["package", "x86_pkg_temp", "tctl", "tdie"],
      Temperature.tempTooltipSensorFilter =
        sensorNameContainsAny ["core", "ccd"]
    }

gpuTemperatureConfig :: Temperature.TemperatureConfig
gpuTemperatureConfig =
  Temperature.defaultTemperatureConfig
    { Temperature.tempFormat = "GPU $temp$\176C",
      Temperature.tempSensorFilter =
        sensorNameContainsAny ["amdgpu", "gpu", "edge", "junction"]
    }

temperatureRow :: TaffyIO Gtk.Widget -> TaffyIO Gtk.Widget
temperatureRow valueBuilder = do
  valueWidget <- valueBuilder
  liftIO $ do
    iconWidget <- Gtk.toWidget =<< Gtk.labelNew (Just ("\xF2C9" :: T.Text))
    row <- buildIconLabelBox iconWidget valueWidget
    widgetSetClassGI row "temperature-row"

gpuTemperatureRow :: TaffyIO Gtk.Widget
gpuTemperatureRow = do
  nvidiaSmiNoWake <- liftIO $ findExecutable "nvidia-smi-no-wake"
  nvidiaSmi <- case nvidiaSmiNoWake of
    Just command -> pure $ Just command
    Nothing -> liftIO $ findExecutable "nvidia-smi"
  temperatureRow $ case nvidiaSmi of
    Just command ->
      NvidiaTemperature.nvidiaTemperatureLabelNewChanWith $
        NvidiaTemperature.defaultNvidiaTemperatureConfig
          { NvidiaTemperature.nvidiaTemperatureCommand = command,
            NvidiaTemperature.nvidiaTemperatureGpuIndex = Just 0
          }
    Nothing -> Temperature.temperatureLabelNewChanWith gpuTemperatureConfig

cpuGpuTemperatureWidget :: TaffyIO Gtk.Widget
cpuGpuTemperatureWidget =
  stackInPill
    "cpu-gpu-temperature"
    [ temperatureRow (Temperature.temperatureLabelNewChanWith cpuTemperatureConfig),
      gpuTemperatureRow
    ]
